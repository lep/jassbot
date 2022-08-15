{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Jassbot.Search (search) where

import Prelude hiding (lookup)

import Control.Arrow (first)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Reader


import Data.Array.ST
import Data.STRef

import Data.Char (toLower)
import Data.Monoid (First(..), getFirst)
import Data.Ord (Down(..))
import Data.List (sortOn, genericLength, elemIndex)
import Data.Maybe (catMaybes)

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Megaparsec (parse)

import Jass.Ast (Type, Name)

import Jassbot.Signature
import Jassbot.DB

type TypeMap = Map Type Type


lookup :: (MonadReader TypeMap m) => Type -> m Type
lookup name = do
    x <- asks $ Map.lookup name
    case x of
        Nothing -> error $ unwords ["Unknown type:", show name]
        Just t -> return t

hasParent a = asks $ Map.member a

ancestors base = do
    hasp <- hasParent base
    if hasp
    then (base:) <$> (ancestors =<< lookup base)
    else return [base]


similarity :: (Fractional b, MonadReader TypeMap m) => Type -> Type -> m b
similarity a b
    | a == b = return 0
similarity a b = do
    idxA <- elemIndex b <$> ancestors a
    idxB <- elemIndex a <$> ancestors b

    case (idxA, idxB) of
        (Nothing, Nothing) -> return 1
        (Just idx, _) -> return $ p idx
        (_, Just idx) -> return $ p idx
  where
    p 1 = 0.1
    p 2 = 0.2
    p 3 = 0.3
    p 4 = 0.4
    p _ = 0.5

rsimilarity a b = (1-) <$> similarity a b


levenshtein :: TypeMap -> [Type] -> [Type] -> Double
levenshtein _ [] [] = 1
levenshtein m a b   = toPercent $ runST x
  where
    maximumScore = max (genericLength a) (genericLength b)
    toPercent v = (maximumScore - v) / maximumScore

    x :: ST s Double
    x = runReaderT y m

    y :: ReaderT TypeMap (ST s) Double
    y = do
        let la = length a
            lb = length b

        d <- lift (newArray ((0, 0), (la, lb)) 0 :: ST s (STUArray s (Int, Int) Double))

        pa <- lift $ newSTRef undefined
        pb <- lift $ newSTRef undefined

        forM_ [0..la] $ \i ->
            lift $ writeArray d (i, 0) $ fromIntegral i * 1.0

        forM_ [0..lb] $ \j ->
            lift $ writeArray d (0, j) $ fromIntegral j * 0.5


        forM_ (zip [0..] b) $ \(j, cb) -> do
            forM_ (zip [0..] a) $ \(i, ca) -> do
                s_score <- similarity ca cb
                --let s_score = if ca == cb then 0 else 1
                dcost <- (+1)       <$> lift (readArray d (i, j+1))
                icost <- (+0.5)     <$> lift (readArray d (i+1, j))
                scost <- (+s_score) <$> lift (readArray d (i, j))

                lift $ writeArray d (i+1, j+1) $ minimum [dcost, icost, scost]

                when (and [i > 0, j > 0]) $ do
                    pa' <- lift $ readSTRef pa
                    pb' <- lift $ readSTRef pb

                    when (ca == pb' && cb == pa') $ do
                        cur <- lift $ readArray d (i+1, j+1)
                        prev <- lift $ readArray d (i-1, j-1)
                        lift (writeArray d (i+1, j+1) $ min cur (prev + 0.2))
                    
                lift $ writeSTRef pa ca
            lift $ writeSTRef pb cb

        lift $ readArray d (la, lb)

fuzzy :: Name -> Name -> (Double, Bool)
fuzzy needle string = first (toPercent . scoreAndEarlyMatchBonus) . go needle string $ Accu 0 False 0 mempty
  where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)

    cons = (:)

    go (uncons -> Nothing)     leftover                !a = (a { score = score a + (genericLength leftover)*leftoverNeedlePenality} , True)
    go needle                  (uncons -> Nothing)     !a = (a, null needle)
    go (uncons -> Just (p,ps)) (uncons -> Just (s,ss)) !a
        | toLower p /= toLower s =
            go (cons p ps) ss $
                Accu { pos = pos a + 1
                     , matchedPrevious = False
                     , score = score a + nonMatchScore
                     , firstMatch = firstMatch a
                     }
        | p == s =
            go ps ss $
                Accu { pos = pos a + 1
                     , matchedPrevious = True
                     , score = score a + caseMatchScore + continuityBonus a
                     , firstMatch = firstMatch a <> First (Just $ pos a)
                     }
        | otherwise = 
            go ps ss $ 
                Accu { pos = pos a + 1
                     , matchedPrevious = True
                     , score = score a + nonCaseMatchScore + continuityBonus a
                     , firstMatch = firstMatch a <> First (Just $ pos a)
                     }

    continuityBonus a =
        if matchedPrevious a
        then continuityScore
        else 0

    toPercent thisScore = 1- (optimalScore - thisScore) / optimalScore
    optimalScore = (genericLength needle -1) * (caseMatchScore + continuityScore) + caseMatchScore + earlyMatchScore
    
    continuityScore     = 1
    nonCaseMatchScore   = 0
    earlyMatchScore     = 1
    nonMatchScore       = 0
    caseMatchScore      = 2
    leftoverNeedlePenality  = -0.05

    scoreAndEarlyMatchBonus accu =
      score accu + 
      if hasEarlyMatch accu
      then earlyMatchScore
      else 0

    hasEarlyMatch accu =
      case getFirst $ firstMatch accu of
        Just x | x < 2 -> True
        _ -> False
 

data FindAccumulator = Accu {
      pos :: Int
    , matchedPrevious :: Bool
    , score :: Double
    , firstMatch :: First Int
    } deriving (Show)



search :: DB -> String -> Double -> [(Double, Signature)]
search db needle threshold =
    let name :: Maybe Name
        params :: Maybe [Type]
        ret :: Maybe Name

        Right (name, params, ret) = parse sloppySignatureParser "input" needle
        
        normalize = 1/genericLength (catMaybes [void name, void params, void ret])

        s x = (score (dbTypes db) name params ret x, x)

    in sortOn (Down . fst)
     . filter ( (>threshold) .fst)
     . map (first (* normalize) . s)
     $ dbSigs db

  where
    score tymap name params ret sig = sum $ map ( $ sig)
            [ getNameSearch name
            , getParamSearch params tymap
            , getReturnSearch ret tymap
            , nativeBonus
            ]

    nativeBonus (Sig _ Native _ _ _) = 0.00
    nativeBonus (Sig _ Function _ _ _) = -0.01

    getNameSearch Nothing _ = 0
    getNameSearch (Just name) sig =
      case fuzzy name $ fnname sig of
        (_, False) -> -1/0
        (score, True) -> score

    getParamSearch Nothing _ _ = 0
    getParamSearch (Just params) tymap sig =
        levenshtein tymap params (parameters sig)

    getReturnSearch Nothing _ _ = 0
    getReturnSearch (Just ret) tymap sig =
        runReader (rsimilarity ret (returnType sig)) tymap
                
