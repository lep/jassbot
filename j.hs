{-# LANGUAGE GADTs #-}

import Options.Applicative

import Control.Applicative
import Control.Monad

import Data.Binary (decodeFileOrFail, encodeFile)

import System.Directory
import System.IO (hPutStrLn, stderr)
import System.IO.Error
import System.Exit
import System.FilePath ((</>))


import Text.Megaparsec (parse, errorBundlePretty)
import Jass.Parser
import Jass.Ast

import Jassbot.Signature (pretty)
import Jassbot.Search (search)
import Jassbot.Typeof (typeof)
import Jassbot.DB (buildDatabase, DB)


data Command = TypeOf String | Search String | MkDatabase String String

parseOptions = customExecParser (prefs showHelpOnEmpty) opts
  where
    opts = info (pCommand <**> helper)
        ( fullDesc
        <> header ("j - cli jass utility")
        )
    pCommand = hsubparser
        ( command "type" (info typeOfOptions (progDesc "Returns the type of the function"))
        <> command "search" (info searchOptions (progDesc "Fuzzy searches functions"))
        <> command "init" (info mkDatabaseOptions (progDesc "Initialized the database used"))
        )

    typeOfOptions =
        TypeOf <$> argument str (help "function name" <> metavar "needle")

    searchOptions =
        Search <$> argument str (help "search string" <> metavar "needle")

    mkDatabaseOptions =
        MkDatabase <$> argument str (help "path to common.j" <> metavar "common.j")
                   <*> argument str (help "path to Blizzard.j" <> metavar "Blizzard.j")

readDb :: IO DB
readDb = do
    datadir <- getXdgDirectory XdgData "jassbot"
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

writeDb :: DB -> IO ()
writeDb db = do
    datadir <- getXdgDirectory XdgData "jassbot"
    createDirectoryIfMissing True datadir
    encodeFile (datadir </> "jassbot.db") db

main = do
    options <- parseOptions
    case options of
        TypeOf needle -> typeOfx needle
        Search needle -> searchx needle
        MkDatabase cj bj -> mkDatabasex cj bj

typeOfx needle = do
    db <- readDb
    let x = typeof db needle
    case x of
        Nothing -> putStrLn $ unwords ["Unknown function:", needle]
        Just s -> putStrLn $ pretty s


searchx needle = do
    db <- readDb

    forM_ (take 3 $ search db needle) $
        putStrLn . pretty . snd

mkDatabasex cj bj = do
    c <- parse programm "common.j" <$> readFile cj
    b <- parse programm "Blizzard.j" <$> readFile bj

    case (c, b) of
        (Right (Programm c'), Right (Programm b')) -> do
            writeDb . buildDatabase . Programm $ c' <> b'

        (Left err, _) -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitWith $ ExitFailure 2
        (_, Left err) -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitWith $ ExitFailure 2





