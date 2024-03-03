{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

import qualified Control.Exception as E
import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Function ((&))
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import GHC.Generics (Generic)
import Jass.Ast
import Jass.Parser (programm)
import Jassbot.Parser
import Jassbot.SearchNG
import Network.HTTP.Types (status200)
import Network.HostAndPort (maybeHostAndPort)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), bind, close, listen, socket)
import Network.Wai (Request (queryString), responseBuilder, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, parse, parseMaybe)

data Config = Config
  { lsInsertionCost :: Int,
    lsDeletionCost :: Int,
    lsSwapingCost :: Int,
    fzContinuityScore :: Int,
    fzNonCaseMatchScore :: Int,
    fzEarlyMatchScore :: Int,
    fzNonMatchScore :: Int,
    fzCaseMatchScore :: Int,
    fzLeftoverNeedlePenality :: Int
  }
  deriving (Show, Read, Generic, FromJSON, ToJSON)

data Options = Options
  { threshold :: Score,
    numResults :: Int,
    serverAddress :: Connection,
    jassFiles :: [FilePath]
  }
  deriving (Show)

data Connection
  = HostAndPort String (Maybe String)
  | Path FilePath
  deriving (Show)

connectionParser str =
  case maybeHostAndPort str of
    Nothing -> Path str
    Just (host, port) -> HostAndPort host port

parseOptions = customExecParser (prefs showHelpOnEmpty) opts
  where
    opts =
      info
        (pCommand <**> helper)
        ( fullDesc
            <> header "j - web api endpoint"
        )
    pCommand =
      Options
        <$> option auto (showDefault <> help "Display entries with at most T score" <> metavar "T" <> value 1500 <> long "limit")
        <*> option auto (showDefault <> help "Display N results at most" <> metavar "N" <> value 20 <> long "num-results")
        <*> (connectionParser <$> strOption (long "address" <> showDefault <> help "Serveraddress. Can either be host:port or path" <> value "127.0.0.1:3000"))
        <*> many (argument str (metavar "FILES..."))

main = do
  -- ast <- parsecj
  -- let (s, db) = setup ast
  -- args <- getArgs
  -- let queryString = unwords args
  -- let p x = parseMaybe (x <* eof)
  --     q = MinQuery $ mapMaybe (`p` queryString) [functionP, typeP, globalP]
  --     topN = db & map (\x -> (x, scoreQuery s q x)) & sortOn snd & take 40

  -- print q
  -- forM_ topN print

  options <- parseOptions
  print options
  x <- runExceptT $ forM (jassFiles options) $ \path -> do
    src <- liftIO $ readFile path
    exceptT $ parse programm path src
  case x of
    Left err -> do
      hPutStrLn stderr $ errorBundlePretty err
      exitWith $ ExitFailure 2
    Right (concatPrograms -> ast) -> do
      let (s, db, allTypes) = setup ast
      runWarpServer s allTypes db options
  where
    getToplevel :: Ast x Programm -> [Ast x Toplevel]
    getToplevel (Programm xs) = xs

    concatPrograms :: [Ast x Programm] -> Ast x Programm
    concatPrograms = Programm . concatMap getToplevel

exceptT :: Either e a -> ExceptT e IO a
exceptT = ExceptT . return

search db allTypes s queryString threshold =
  let p x = parseMaybe $ x <* eof
      q = MinQuery $ mapMaybe (`p` queryString) [functionP, typeP, globalP]
      q' = fuzzyType allTypes q
      scored = filter ((<= threshold) . snd) $ map (\x -> (x, scoreQuery s q' x)) db
      sorted = map fst $ sortOn snd scored
   in sorted

-- TODO: this needs caching. Maybe everything does.
fuzzyType allTypes = go
  where
    go = \case
      MinQuery xs -> MinQuery $ map go xs
      SumQuery xs -> SumQuery $ map go xs
      ParamQuery xs -> ParamQuery $ map (findFuzzyType allTypes) xs
      ExtendsQuery x -> ExtendsQuery $ findFuzzyType allTypes x
      ReturnQuery x -> ReturnQuery $ findFuzzyType allTypes x
      x -> x


runWarpServer s allTypes db options =
  case serverAddress options of
    Path path -> E.bracket (mkSock path) (rmSock path) runWithSock
    HostAndPort host (Just port) -> do
      let settings = setHost (fromString host) $ setPort (read port) defaultSettings
      runSettings settings $ app db options
  where
    mkSock path = do
      sock <- socket AF_UNIX Stream 0
      bind sock (SockAddrUnix path)
      listen sock 5
      pure sock

    rmSock path sock = do
      close sock
      removeFile path

    runWithSock sock =
      runSettingsSocket defaultSettings sock $ app db options

    app db options req respond =
      case lookup "q" $ queryString req of
        Just (Just query)
          | not $ BS.null query ->
              respond
                $ responseLBS
                  status200
                  [ ("Content-Type", "text/json"),
                    ("Access-Control-Allow-Origin", "*") -- only used for local dev
                  ]
                $ encode
                $ map pretty . take (numResults options)
                $ search db allTypes s (UTF8.toString query) (threshold options)
        _ ->
          respond $
            responseBuilder
              status200
              [ ("Access-Control-Allow-Origin", "*"), -- only used for local dev
                ("Content-Type", "text/json")
              ]
              "[]"
