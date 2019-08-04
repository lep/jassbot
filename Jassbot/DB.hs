{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

module Jassbot.DB (DB(..), buildDatabase) where

import GHC.Generics
import Data.Binary

import Data.Map (Map)
import qualified Data.Map as Map

import Jass.Ast (Type, Name)
import qualified Jass.Ast as Jass
import Data.Composeable

import Jassbot.Signature

type TypeMap = Map Type Type


data DB = DB { dbTypes :: Map Type Type
             , dbSigs :: [Signature]
             } deriving (Generic)

instance Binary DB


buildDatabase :: Jass.Ast Name x -> DB
buildDatabase = uncurry DB . go mempty mempty
  where
    go :: TypeMap -> [Signature] -> Jass.Ast Name x -> (TypeMap, [Signature])
    go tymap sigs x =
      case x of
        Jass.Typedef base new ->
            let tymap' = Map.insert base new tymap
            in (tymap', sigs)

        Jass.Native c name args ret ->
            let sig = Sig c Native name args ret
            in (tymap, sig:sigs)

        Jass.Function c name args ret _ ->
            let sig = Sig c Function name args ret
            in (tymap, sig:sigs)

        _ -> composeFold (go tymap sigs) x

