{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use &&" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Jassbot.SearchNG where

import Control.Monad (forM_, when)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.ST
import Control.Monad.Trans (lift)
import Data.Array.IO (readArray, writeArray)
import Data.Array.ST (MArray (newArray), STUArray)
import Data.Bifunctor (first)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Char (toLower)
import Data.List (genericLength, intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (First (..), getFirst)
import Data.Ord (Down (Down))
import Data.Relation (Relation)
import qualified Data.Relation as Relation
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Jass.Ast (Ast (ADef, Function, Native, Programm, SDef, Typedef), Constant (..), Programm, Toplevel)
import qualified Jass.Ast as Jass
import qualified Jass.Parser as Jass
import qualified Jass.Printer as Jass
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (errorBundlePretty, parse)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (any)

similarity = undefined

parsecj :: IO (Ast Name Programm)
parsecj = do
  j <- readFile "common.j"
  b <- readFile "blizzard.j"
  let Right (Jass.Programm cj) = parse Jass.programm "common.j" j
      Right (Jass.Programm bj) = parse Jass.programm "blizzarrd.j" b
  pure $ Jass.Programm $ cj ++ bj

ast2typeRelation :: Ast Name Programm -> Relation Type Type
ast2typeRelation (Programm ts) = Relation.fromList $ mapMaybe go ts
  where
    go :: Ast Name Toplevel -> Maybe (Type, Type)
    go x =
      case x of
        Typedef from to -> Just (from, to)
        _ -> Nothing

ast2searchableThing :: Ast Name Programm -> [SearchableThing]
ast2searchableThing (Programm ts) = map go ts
  where
    go :: Ast Name Toplevel -> SearchableThing
    go x =
      case x of
        Native cnst name args ret -> Signature cnst "native" name args ret
        Function cnst name args ret _ -> Signature cnst "function" name args ret
        Typedef from to -> TypeDef from to
        Jass.Global vdef ->
          case vdef of
            ADef name ty -> Global Normal True ty name ""
            SDef cnst name ty i ->
              Global cnst False ty name $
                UTF8.toString . toLazyByteString $
                  maybe mempty (\x -> "= " <> Jass.printExpr x) i

setup ast =
  -- ast <- parsecj
  let tyRel = Relation.insert "integer" "real" $ ast2typeRelation ast
      db = ast2searchableThing ast ++ map BuiltInType ["handle", "code", "integer", "real", "string", "boolean"]
      allTypes =
        Set.toList $
          Relation.ran tyRel
            `Set.union` Relation.dom tyRel
            `Set.union` Set.fromList ["handle", "code", "integer", "real", "string", "boolean", "nothing"] -- nothing? only needed for fuzzy type completion i think
      cfg =
        Config
          { insertionCost = const 170,
            deletionCost = const 70,
            equalityScore = \a b ->
              let co = covariantTypeDistance tyRel a b
                  contra = contravariantTypeDistance tyRel a b
                  -- bascially checking for Nothing
                  co' = if co >= scoreMaxBound then co else min 4 co
                  contra' = if contra >= scoreMaxBound then contra else min 4 contra
               in 40 * min co' (contra' + 2),
            swappingCost = \_ _ -> 20
          }

      s = S tyRel (dls cfg)
   in (s, db, allTypes)

-- pure (s, db)

q1 =
  MinQuery
    [ ParamQuery ["integer", "real"],
      SumQuery
        [ ReturnQuery "integer",
          NameQuery "real"
        ]
    ]

q2 =
  SumQuery
    [ qa,
      qb
    ]

qa = NameQuery "Unit"

qb = ParamQuery ["unit", "integer", "integer", "real"]

data SearchableThing
  = Signature Constant String Name [(Type, Name)] Type
  | TypeDef Name Name
  | BuiltInType Name
  | Global Constant Bool Type Name String
  deriving (Show)

pretty :: SearchableThing -> String
pretty = \case
  Global constant isArray ty name i ->
    unwords $
      filter
        (not . null)
        [ pconst constant,
          ty,
          parray isArray,
          name,
          i
        ]
  TypeDef from to -> unwords ["type", from, "extends", to]
  BuiltInType ty -> unwords ["type", ty]
  Signature c ty n args r ->
    unwords $
      filter
        (not . null)
        [ pconst c,
          ty,
          n,
          "takes",
          ptakes args,
          "returns",
          r
        ]
  where
    ptakes [] = "nothing"
    ptakes xs = intercalate ", " $ map (\(a, b) -> unwords [a, b]) xs
    pconst Const = "constant"
    pconst Normal = ""

    parray True = "array"
    parray False = ""

nameOf x =
  case x of
    Signature _ _ name' _ _ -> name'
    TypeDef name' _ -> name'
    BuiltInType name' -> name'
    Global _ _ _ name' _ -> name'

data Query
  = NameQuery Name
  | ParamQuery [Type]
  | ReturnQuery Type
  | ExtendsQuery Type
  | MinQuery [Query]
  | SumQuery [Query]
  | EmptyQuery
  deriving (Eq, Show)

data Filter
  = IsConstant
  | IsArray
  | HasSignature
  | IsTypeDef
  deriving (Eq)

-- parse = \case
--   ["global", ty] -> undefined
--   ["global", ty, "array"] -> undefined
--   ["global", ty, "array", name] -> undefined
--   ["global", ty, name] -> undefined

--   ["extend", ty] -> undefined
--   ["extends", ty] -> undefined

--   []

type Score = Int

type TypeRelation = Relation Type Type

findFuzzyType :: [Type] -> Type -> Type
findFuzzyType haystack needle = head $ sortOn (fuzzy needle) haystack

-- varianceAwareFuzzyTypeSearch fn haystack needle =
--   let (bestFuzzyFit, fzyScore) = head $ sortOn snd $ map (\x -> (x, fuzzy needle x)) haystack
--       scoreFn x =
--         let score = distanceBetweenTypes fn x bestFuzzyFit
--         in (x, score + fzyScore)
--   in map scoreFn haystack

-- -- contravariantFuzzyTypeSearch :: Types -> [Type] -> Type -> [(Type, Score)]
-- contravariantFuzzyTypeSearch,  covariantFuzzyTypeSearch :: TypeRelation -> [Type] -> Name -> [(Type, Score)]
-- contravariantFuzzyTypeSearch types = varianceAwareFuzzyTypeSearch (`Relation.lookupDom` types)
-- covariantFuzzyTypeSearch types = varianceAwareFuzzyTypeSearch (`Relation.lookupRan` types)

covariantTypeDistance :: Relation [Char] [Char] -> [Char] -> [Char] -> Score
covariantTypeDistance tyRel = distanceBetweenTypes (`Relation.lookupRan` tyRel)

contravariantTypeDistance tyRel = distanceBetweenTypes (`Relation.lookupDom` tyRel)

data S = S
  { typeRelation :: TypeRelation,
    tyLevenshtein :: [Type] -> [Type] -> Score
  }

scoreQuery :: S -> Query -> SearchableThing -> Score
scoreQuery s q t =
  case q of
    NameQuery {} -> scoreNameQuery q t
    ReturnQuery {} -> scoreReturnQuery q t
    ParamQuery {} -> scoreParamQuery q t
    ExtendsQuery {} -> scoreExtendsQuery q t
    MinQuery qs ->
      case filter (/= EmptyQuery) qs of
        [] -> scoreMaxBound
        qs' -> minimum $ map (\q' -> scoreQuery s q' t) qs'
    SumQuery qs -> sum $ map (\q' -> scoreQuery s q' t) $ filter (/= EmptyQuery) qs
    EmptyQuery -> scoreMaxBound
  where
    scoreNameQuery (NameQuery name) t = fuzzy name $ nameOf t

    scoreReturnQuery (ReturnQuery ty) = \case
      Signature _ _ _ _ returnTy -> covariantTypeDistance (typeRelation s) ty returnTy
      Global _ _ globalTy _ _ -> covariantTypeDistance (typeRelation s) ty globalTy
      _ -> scoreMaxBound

    scoreParamQuery (ParamQuery ts) = \case
      Signature _ _ _ params _ -> tyLevenshtein s ts $ map fst params
      Global _ True globalTy _ _ -> tyLevenshtein s ts ["integer"]
      _ -> scoreMaxBound

    scoreExtendsQuery (ExtendsQuery ty) = \case
      TypeDef newName _ -> covariantTypeDistance (typeRelation s) ty newName
      BuiltInType tyName -> covariantTypeDistance (typeRelation s) ty tyName
      _ -> scoreMaxBound

scoreMaxBound :: Score
scoreMaxBound = 100000

applyFilter f t =
  case f of
    IsTypeDef ->
      case t of
        TypeDef {} -> True
        BuiltInType {} -> True
        _ -> False
    HasSignature ->
      case t of
        Signature {} -> True
        _ -> False
    IsArray ->
      case t of
        Global _ True _ _ _ -> True
        _ -> False
    IsConstant ->
      case t of
        Global Const _ _ _ _ -> True
        _ -> False

type Types = Relation Type Type

-- dir either Relation.lookupDom or Relation.lookupRan
distanceBetweenTypes :: (Type -> Set Type) -> Type -> Type -> Score
-- distanceBetweenTypes _ "integer" "real" = 1
distanceBetweenTypes _ x y | x == y = 0
distanceBetweenTypes dir from to = go 1 (dir from)
  where
    -- go :: Int -> Set Type -> Maybe Int
    go !cnt lvl
      | Set.null lvl = scoreMaxBound
      | to `Set.member` lvl = cnt
      | otherwise =
          let lvl' = Set.unions $ map dir $ Set.toList lvl
           in go (succ cnt) lvl'

type Name = String

type Type = String

fuzzy :: Name -> Name -> Score
fuzzy pattern string =
  let (score, matching) = go pattern string $ Accu 0 False 0 mempty
   in if matching
        then lowerIsBetter $ scoreAndEarlyMatchBonus score
        else scoreMaxBound
  where
    uncons [] = Nothing
    uncons (x : xs) = Just (x, xs)

    cons = (:)

    go :: Name -> Name -> FindAccumulator -> (FindAccumulator, Bool)
    go (uncons -> Nothing) leftover !a = (a {score = score a + genericLength leftover * leftoverNeedlePenality}, True)
    go needle (uncons -> Nothing) !a = (a, null needle)
    go (uncons -> Just (p, ps)) (uncons -> Just (s, ss)) !a
      | toLower p /= toLower s =
          go (cons p ps) ss $
            Accu
              { pos = pos a + 1,
                matchedPrevious = False,
                score = score a + nonMatchScore,
                firstMatch = firstMatch a
              }
      | p == s =
          go ps ss $
            Accu
              { pos = pos a + 1,
                matchedPrevious = True,
                score = score a + caseMatchScore + continuityBonus a,
                firstMatch = firstMatch a <> First (Just $ pos a)
              }
      | otherwise =
          go ps ss $
            Accu
              { pos = pos a + 1,
                matchedPrevious = True,
                score = score a + nonCaseMatchScore + continuityBonus a,
                firstMatch = firstMatch a <> First (Just $ pos a)
              }

    continuityBonus a =
      if matchedPrevious a
        then continuityScore
        else 0

    lowerIsBetter x = optimalScore - x
    optimalScore = (genericLength pattern - 1) * (caseMatchScore + continuityScore) + caseMatchScore + earlyMatchScore

    continuityScore = 100
    nonCaseMatchScore = 0
    earlyMatchScore = 100
    nonMatchScore = 0
    caseMatchScore = 200
    leftoverNeedlePenality = -1

    scoreAndEarlyMatchBonus accu =
      score accu
        + if hasEarlyMatch accu
          then earlyMatchScore
          else 0

    hasEarlyMatch accu =
      case getFirst $ firstMatch accu of
        Just x | x < 2 -> True
        _ -> False

data FindAccumulator = Accu
  { pos :: Int,
    matchedPrevious :: Bool,
    score :: Score,
    firstMatch :: First Int
  }
  deriving (Show)

type TypeMap = Map Type Type

data LevenstheinConfig a = Config
  { insertionCost :: a -> Int,
    deletionCost :: a -> Int,
    equalityScore :: a -> a -> Int, -- 0 should mean equality
    swappingCost :: a -> a -> Int
  }

dls cfg b a = runST go
  where
    go :: ST s Int
    go = do
      let aLen = length a
          bLen = length b

      aPrevRef <- newSTRef undefined
      bPrevRef <- newSTRef undefined

      d <- newArray ((0, 0), (aLen, bLen)) 0 :: ST s (STUArray s (Int, Int) Int)

      writeArray d (0, 0) 0

      forM_ (zip [1 ..] a) $ \(i, ca) ->
        writeArray d (i, 0) $ i * deletionCost cfg ca

      forM_ (zip [1 ..] b) $ \(j, cb) ->
        writeArray d (0, j) $ j * insertionCost cfg cb

      forM_ (zip [1 ..] b) $ \(j, cb) -> do
        forM_ (zip [1 ..] a) $ \(i, ca) -> do
          dCost <- readArray d (i - 1, j)
          iCost <- readArray d (i, j - 1)
          sCost <- readArray d (i - 1, j - 1)

          let sScore = equalityScore cfg ca cb
              dScore = deletionCost cfg ca
              iScore = insertionCost cfg cb

              cost = minimum [dCost + dScore, iCost + iScore, sCost + sScore]

          writeArray d (i, j) cost

          when (and [i > 1, j > 1]) $ do
            aPrev <- readSTRef aPrevRef
            bPrev <- readSTRef bPrevRef

            when (ca == bPrev && cb == aPrev) $ do
              let swapScore = swappingCost cfg ca cb
              prevCost <- readArray d (i - 2, j - 2)
              writeArray d (i, j) $ min cost (prevCost + swapScore)

          writeSTRef aPrevRef ca
        writeSTRef bPrevRef cb
      readArray d (aLen, bLen)
