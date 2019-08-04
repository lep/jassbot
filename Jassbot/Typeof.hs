

module Jassbot.Typeof where

import Data.List (find)

import Jassbot.Signature
import Jassbot.DB

typeof :: DB -> String -> Maybe Signature
typeof db needle = find ( (== needle) . fnname ) (dbSigs db)
