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
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)

import Options.Applicative

import Data.List (intercalate)
import qualified Jass.Ast

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
    , databasePath :: Maybe FilePath
    } deriving (Show)

parseOptions = customExecParser (prefs showHelpOnEmpty) opts
  where
    opts = info (pCommand <**> helper)
        ( fullDesc
        <> header "j - web api endpoint"
        )
    pCommand =
        Options <$> option auto (showDefault <> help "Display entries with at least T score" <> metavar "T" <> value 0.4 <> long "threshold")
                <*> option auto (showDefault <> help "Display N results at most" <> metavar "N" <> value 20 <> long "num-results")
                <*> optional (option str $ long "data-dir")

main = do
    options <- parseOptions
    db <- readDb $ databasePath options
    runServer (answerOnce options db)
  where
    answerOnce options db sock = do
        query <- recv sock 4096
        unless (S.null query) $ do
            sendAll sock
            . S8.pack
            . (\x -> "[" ++ x ++ "]") -- We do this by hand as compiling aeson
            . intercalate ","         -- takes too much memory on my VPS.
            . map (show.pretty.snd)
            . take (numResults options)
            . search db (UTF8.toString query) $ threshold options

-- adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html
runServer server = E.bracket mkSock rmSock loop
  where
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _) -> void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)
    mkSock = do
        sock <- socket AF_UNIX Stream 0
        bind sock (SockAddrUnix "/tmp/jassbot-api.sock")
        listen sock 5
        pure sock

    rmSock sock = do
        close sock
        removeFile "/tmp/jassbot-api.sock"

