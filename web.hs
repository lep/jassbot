{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
import Jassbot.Search
import Jassbot.DB
import Jassbot.Signature

import Data.Binary (decodeFileOrFail)

import System.Directory
import System.IO (hPutStrLn, stderr)
import System.IO.Error
import System.Exit
import System.FilePath ((</>))
import System.Environment


import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void, forM)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)

import Options.Applicative

import Data.List (intercalate)
import qualified Jass.Ast
import Text.Megaparsec (errorBundlePretty, parse)
import Jass.Parser (programm)
import Control.Monad.Except (runExceptT, ExceptT(..))
import Control.Monad.Trans.Except (except)
import Control.Monad.IO.Class (liftIO)

import Network.HostAndPort (maybeHostAndPort)
import Network.Wai (responseBuilder, responseLBS, queryString)
import Network.Wai.Handler.Warp (run, runSettingsSocket, defaultSettings, runSettings, setHost, setPort)
import Network.HTTP.Types (status200)
import Data.Aeson (encode)
import Data.Binary.Builder (fromLazyByteString)
import qualified Data.ByteString as BS
import Data.String (fromString)

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

data Options =
  Options
    { threshold :: Double
    , numResults :: Int
    , serverAddress :: Connection
    , jassFiles :: [FilePath]
    } deriving (Show)

data Connection =
      HostAndPort String (Maybe String)
    | Path FilePath
    deriving (Show)

connectionParser str =
    case maybeHostAndPort str of
        Nothing -> Path str
        Just (host, port) -> HostAndPort host port


parseOptions = customExecParser (prefs showHelpOnEmpty) opts
  where
    opts = info (pCommand <**> helper)
        ( fullDesc
        <> header "j - web api endpoint"
        )
    pCommand =
        Options <$> option auto (showDefault <> help "Display entries with at least T score" <> metavar "T" <> value 0.4 <> long "threshold")
                <*> option auto (showDefault <> help "Display N results at most" <> metavar "N" <> value 20 <> long "num-results")
                -- <*> optional ( strOption (showDefault <> help "Path to the database" <> metavar "DB") )
                <*> (connectionParser <$> strOption (long "address" <> showDefault <> help "Serveraddress. Can either be host" <> value "127.0.0.1:3000"))
                <*> many (argument str (metavar "FILES..."))

-- except :: Either e a -> ExceptT e IO a
-- except = ExceptT . return

concatPrograms :: [Jass.Ast.Ast x Jass.Ast.Programm] -> Jass.Ast.Ast x Jass.Ast.Programm
concatPrograms = Jass.Ast.Programm . concatMap getToplevel

getToplevel :: Jass.Ast.Ast x Jass.Ast.Programm -> [Jass.Ast.Ast x Jass.Ast.Toplevel]
getToplevel (Jass.Ast.Programm xs) = xs

buildDB :: [FilePath] -> IO DB
buildDB files = do
    x <- runExceptT $ forM files $ \path -> do
        src <- liftIO $ readFile path
        except $ parse programm path $ src <> "\n"
    case x of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitWith $ ExitFailure 2
        Right progs -> pure $ buildDatabase $ concatPrograms progs



main = do
    options <- parseOptions
    -- db <- getDB (jassFiles options) (dbPath options)
    db <- buildDB (jassFiles options)
    runWarpServer db options
    -- case serverAddress options of
    --     Path path -> runServer path (answerOnce options db)
    --     HostAndPort host port -> runWarpServer db options
  -- where
  --   answerOnce options db sock = do
  --       query <- recv sock 4096
  --       unless (S.null query) $ do
  --           sendAll sock
  --           . S8.pack
  --           . (\x -> "[" ++ x ++ "]") -- We do this by hand as compiling aeson
  --           . intercalate ","         -- takes too much memory on my VPS.
  --           . map (show.pretty.snd)
  --           . take (numResults options)
  --           . search db (UTF8.toString query) $ threshold options


runWarpServer db options  =
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
            Just (Just query) | not $ BS.null query -> respond $
                responseLBS status200 [ ("Content-Type", "text/json")] $
                    encode $
                     map (pretty.snd)
                    . take (numResults options)
                    . search db (UTF8.toString query) $ threshold options
            _ -> respond $ responseBuilder status200 [ ("Content-Type", "text/json")] "[]"

-- adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html
runServer path server = E.bracket mkSock rmSock loop
  where
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _) -> void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)
    mkSock = do
        sock <- socket AF_UNIX Stream 0
        bind sock (SockAddrUnix path)
        listen sock 5
        pure sock

    rmSock sock = do
        close sock
        removeFile path

