module Jassbot.Parser where

import Control.Monad (guard, void)
import Data.Functor (($>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Jassbot.SearchNG (Query (..))
import Text.Megaparsec
import Text.Megaparsec.Char (hspace, letterChar, string, alphaNumChar, char)
import Text.Megaparsec.Debug
import Prelude hiding (any)

{-
any = const True

tok n = (== n)

type Delta = IntMap [(String -> Bool, Int)]

delta :: Delta
delta =
  Map.fromList
    [ ( 0,
        [ (tok "type", 1),
          (tok "global", 8),
          (tok "takes", 4),
          (any, 3)
        ]
      ),
      (1, [(any, 2)]),
      ( 3,
        [ (tok "takes", 4),
          (tok "returns", 6),
          (tok "array", 10),
          (any, 12)
        ]
      ),
      (4, [(any, 5)]),
      ( 5,
        [ (tok "returns", 6),
          (any, 5)
        ]
      ),
      (6, [(any, 7)]),
      (8, [(any, 9)]),
      (9, [(any, 10)]),
      (10, [(any, 11)]),
      (12, [(any, 12)])
    ]

q0 = Set.singleton 0

f :: Set Int
f = Set.fromList [2, 3, 5, 7, 9, 10, 11, 12]

transition :: Delta -> [Int] -> String -> [Int]
transition delta qs input = do
  q <- qs
  (pred, q') <- Map.findWithDefault [] q delta
  guard $ pred input
  pure q'

apply :: Delta -> Set Int -> [Int] -> [String] -> [Int]
apply delta f q [] = filter (`Set.member` f) q
apply delta f q (x : xs) = apply delta f (transition delta q x) xs
-}

type Parser = Parsec Void String

identifier :: Parser String
identifier = try $ do
  i <- some (alphaNumChar <|> char '_')
  -- guard $ i `notElem` reservedNames
  hspace
  pure i
  where
    reservedNames = ["native", "function", "global", "type", "extends", "array", "takes", "returns"]

reserved :: String -> Parser ()
reserved t = try $ do
  string t
  lookAhead $ notFollowedBy letterChar
  void hspace

anyTok :: Parser String
anyTok = some letterChar <* hspace

singlenameParamP :: Parser Query
singlenameParamP = ParamQuery . pure <$> identifier

singlenameReturnP :: Parser Query
singlenameReturnP = ReturnQuery <$> identifier

{-
type a
type a extends b
a extends b
extends b
-}
typeP :: Parser Query
typeP = tyP <|> try extends1 <|> extends2
  where
    tyP = do
      reserved "type"
      n <- identifier
      q <- option EmptyQuery extends2
      pure $ SumQuery [NameQuery n, q]

    extends1 = do
      x <- identifier
      q <- extends2
      pure $ SumQuery [NameQuery x, q]

    extends2 = do
      reserved "extend" <|> reserved "extends"
      ExtendsQuery <$> identifier

{-
a takes b returns c
a takes b
a returns c
takes b returns c
takes b
returns c
-}

anythingBut ks = try $ do
  x <- identifier
  guard $ x `notElem` ks
  hspace
  pure x

-- not very happy with this but it seems to what it ought to do
functionP :: Parser Query
functionP = do
  x <- identifier
  case x of
    "takes" -> do
      args <- some (anythingBut ["returns"])
      r <- option EmptyQuery $ do
        reserved "returns"
        ReturnQuery <$> identifier
      pure $ SumQuery [ParamQuery args, r]
    "returns" -> ReturnQuery <$> identifier
    _ -> do
      y <- many (anythingBut ["takes", "returns"])
      case y of
        [] -> do
          t <- takesP
          r <- returnsP
          pure $ SumQuery [NameQuery x, t, r]
        _ -> do
          r <- returnsP
          pure $ SumQuery [ParamQuery $ x : y, r]
  where
    takesP = option EmptyQuery $ do
      reserved "takes"
      args <- some (anythingBut ["returns"])
      pure $ ParamQuery args

    returnsP = option EmptyQuery $ do
      reserved "returns"
      ReturnQuery <$> identifier
    asType x = dbg "asType" $ do
      args <- many (anythingBut ["returns"])
      r <- option EmptyQuery $ do
        reserved "returns"
        ReturnQuery <$> identifier
      pure $ SumQuery [ParamQuery $ x : args, r]
    asName x = dbg "asName" $ do
      a <- option EmptyQuery $ do
        reserved "takes"
        args <- some (anythingBut ["returns"])
        pure $ ParamQuery args
      r <- option EmptyQuery $ do
        reserved "returns"
        ReturnQuery <$> identifier
      pure $ SumQuery [NameQuery x, a, r]

-- functionP = name1 <|> name2 <|> takes1 <|> returns1
--   where
--     name1 = do
--       reserved "native" <|> reserved "function"
--       n <- option (EmptyQuery) $ NameQuery <$> identifier
--       t <- option (EmptyQuery) takesOnly
--       r <- option (EmptyQuery) returns1
--       pure $ SumQuery [n, t, r]

--     takesOnly = do
--       reserved "takes"
--       args <- some identifier
--       pure $ ParamQuery args

--     name2 = do
--       n <- identifier
--       t <- optional takes1
--       r <- optional returns1

--       case (t, r) of
--         (Nothing, Nothing) -> empty
--         _ -> pure $ SumQuery $ NameQuery n : catMaybes [t, r]

--     takes1 = do
--       q <- takesOnly
--       r <- option (EmptyQuery) returns1
--       pure $ SumQuery [q, r]

--     returns1 = do
--       reserved "returns"
--       ReturnQuery <$> identifier

{-
a b
a array
array b
a array b
-}
globalP = do
  a <- identifier
  b <- identifier
  c <- optional identifier
  case (a, b, c) of
    ("array", _, Nothing) ->
      pure $
        SumQuery [NameQuery b, ParamQuery ["integer"]]
    (_, "array", Nothing) ->
      pure $
        SumQuery [ReturnQuery a, ParamQuery ["integer"]]
    (_, _, Nothing) ->
      pure $
        SumQuery [NameQuery b, ReturnQuery a]
    (_, "array", Just c') ->
      pure $
        SumQuery [NameQuery c', ReturnQuery a, ParamQuery ["integer"]]
    _ -> empty

-- globalP = dbg "global" $ globalPrefixed <|> arrayP
--   where
--     globalPrefixed =
--       reserved "global" *> (try arrayP <|> singleP)

--     singleP = dbg "single" $ do
--       maybeType <- optional identifier
--       maybeName <- optional identifier
--       pure $ case (maybeType, maybeName) of
--         (Nothing, Nothing) -> EmptyQuery
--         (Just ty, Nothing) -> ReturnQuery ty
--         (Nothing, Just name) -> NameQuery name
--         (Just ty, Just name) -> SumQuery [NameQuery name, ReturnQuery ty]

--     arrayP = dbg "array" $ do
--       maybeType <- optional identifier
--       reserved "array"
--       maybeName <- optional identifier

--       pure $ case (maybeType, maybeName) of
--         (Nothing, Nothing) -> ParamQuery ["integer"]
--         (Just ty, Nothing) -> SumQuery [ReturnQuery ty, ParamQuery ["integer"]]
--         (Nothing, Just name) -> SumQuery [NameQuery name, ParamQuery ["integer"]]
--         (Just ty, Just name) -> SumQuery [NameQuery name, ReturnQuery ty, ParamQuery ["integer"]]
