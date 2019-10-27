{-# LANGUAGE DeriveGeneric #-}

module Jassbot.Signature
    ( Native(..)
    , Signature(..)
    , parameters
    , fnname
    , returnType
    , sloppySignatureParser
    , pretty
    ) where


import GHC.Generics

import Data.Binary

import Data.List (intercalate)

import Data.Functor
import Control.Applicative

import Text.Megaparsec (Parsec)
import Text.Megaparsec ( option, sepBy, try, lookAhead, choice, eof)
import Data.Void

import Jass.Parser as Jass
import qualified Jass.Ast as Jass

data Native = Native | Function
    deriving (Show, Generic)

instance Binary Native

type Name = Jass.Name

data Signature = Sig Jass.Constant Native Name [(Jass.Type, Name)] Jass.Type
    deriving (Show, Generic)

instance Binary Signature


parameters :: Signature -> [Jass.Type]
parameters (Sig _ _ _ p _) = map fst p

fnname :: Signature -> Name
fnname (Sig _ _ n _ _) = n

returnType :: Signature -> Jass.Type
returnType (Sig _ _ _ _ r) = r

sloppySignatureParser :: Parsec Void String (Maybe String, Maybe [String], Maybe String)
sloppySignatureParser =
    justName <|> fullSig <|> nameRet <|> paramRet
  where
    fullSig = try $ do
        --traceM "fullsig"
        name <- Just <$> Jass.identifier
        Jass.reserved "takes"
        args <- (Jass.reserved "nothing" $> ["nothing"]) <|> (Jass.identifier `sepBy` optional (symbol ","))
        ret <- option "" $ do
            Jass.reserved "returns"
            option "" ((Jass.reserved "nothing" $> "nothing") <|> Jass.identifier)

        return $ case args of
            ["nothing"] -> (name, Just [], empty2Maybe ret)
            _           -> (name, empty2Maybe args, empty2Maybe ret)

    nameRet = try $ do
        --traceM "nameRet"
        name <- Just <$> Jass.identifier
        Jass.reserved "returns"
        ret <- option "" ((Jass.reserved "nothing" $> "nothing") <|> Jass.identifier)
        return (name, Nothing, empty2Maybe ret)

    paramRet = try $ do
        --traceM "paramRet"
        optional $ Jass.reserved "takes"
        args <- (Jass.reserved "nothing" $> ["nothing"]) <|> (Jass.identifier `sepBy` optional (symbol ","))
        ret <- option "" $ do
            Jass.reserved "returns"
            option "" ((Jass.reserved "nothing" $> "nothing") <|> Jass.identifier)
    
        return $ case args of
            ["nothing"] -> (Nothing, Just [], empty2Maybe ret)
            _           -> (Nothing, empty2Maybe args, empty2Maybe ret)

    justName = try $ do
        --traceM "justname"
        name <- Jass.identifier <* eof
        return (Just name, Nothing, Nothing)



    empty2Maybe x
        | x == mempty = Nothing
        | otherwise   = Just x

pretty s =
    unwords [ ppconst s ++ ppfn s
            , fnname s, "takes"
            , ppargs $ parameters s
            , "returns", pptype $ returnType s ]
  where
    ppargs [] = "nothing"
    ppargs _ = intercalate ", " $ map pparg (parameters' s)
    pparg (t, n) = unwords [pptype t, n]
    pptype = id

    ppconst (Sig Jass.Const _ _ _ _) = "constant "
    ppconst (Sig Jass.Normal _ _ _ _) = ""

    ppfn (Sig _ Native _ _ _) = "native"
    ppfn (Sig _ Function _ _ _) = "function"

    parameters' (Sig _ _ _ args _) = args
