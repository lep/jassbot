{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)

import Jassbot.Search
import Jassbot.DB
import Jassbot.Signature

import Data.Binary (decodeFileOrFail, encodeFile)

import System.Directory
import System.IO (hPutStrLn, stderr)
import System.IO.Error
import System.Exit
import System.FilePath ((</>))
import System.Environment

import Control.Monad.IO.Class (liftIO)
import Control.Arrow

import Data.Aeson.Types

import qualified Jass.Ast

instance ToJSON Jass.Ast.Constant
instance ToJSON Native
instance ToJSON Signature

getDbPath :: Maybe String -> IO String
getDbPath x =
  case x of
    Just datadir -> return datadir
    Nothing -> getXdgDirectory XdgData "jassbot"

readDb :: Maybe String -> IO DB
readDb p = do
    datadir <- getDbPath p
    x <- tryIOError (decodeFileOrFail $ datadir </> "jassbot.db")
    case x of
        Left ex -> do
            hPutStrLn stderr $ unwords ["Could not open database. Have you run init yet?", show ex]
            exitWith $ ExitFailure 1
        Right x' ->
          case x' of
            Right x' -> return x'
            Left (_, msg) -> do
                hPutStrLn stderr $ unwords ["Could not open database. Have you run init yet?", msg]
                exitWith $ ExitFailure 1

main = do
    path <- Just . head <$> getArgs
    db <- readDb path
    scotty 3000 $
        get "/api" $
            param "query" >>= json . take 20 . (search db `flip` 0.4)

