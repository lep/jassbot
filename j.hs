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


data Command = TypeOf String (Maybe String) | Search String Int Double (Maybe String) | MkDatabase String String (Maybe String)

parseOptions = customExecParser (prefs showHelpOnEmpty) opts
  where
    opts = info (pCommand <**> helper)
        ( fullDesc
        <> header "j - cli jass utility"
        )
    pCommand = hsubparser
        ( command "type" (info typeOfOptions (progDesc "Returns the type of the function"))
        <> command "search" (info searchOptions (progDesc "Fuzzy searches functions"))
        <> command "init" (info mkDatabaseOptions (progDesc "Initialized the database used"))
        )

    typeOfOptions =
        TypeOf <$> argument str (help "The functions name" <> metavar "needle")
               <*> optional (option str $ long "data-dir")

    searchOptions =
        Search <$> argument str (help "The search string" <> metavar "needle")
               <*> option auto ( showDefault
                               <> value 3
                               <> long "results"
                               <> short 'n'
                               <> help "How many results should be displayed at max. Use 0 to disable the limit."
                               <> metavar "num-results"
                               )
               <*> option auto ( showDefault 
                               <> value 0.4
                               <> long "threshold"
                               <> short 't'
                               <> help "Minimum score for a function to be displayed"
                               <> metavar "threshold"
                               )
               <*> optional (option str $ long "data-dir")

    mkDatabaseOptions =
        MkDatabase <$> argument str (help "path to common.j" <> metavar "common.j")
                   <*> argument str (help "path to Blizzard.j" <> metavar "Blizzard.j")
                   <*> optional (option str $ long "data-dir")


main = do
    options <- parseOptions
    case options of
        TypeOf needle p -> readDb p >>= typeOfx needle
        Search _ _ _ p -> readDb p >>= searchx options
        MkDatabase cj bj p -> mkDatabasex cj bj p

typeOfx needle db = do
    let x = typeof db needle
    case x of
        Nothing -> putStrLn $ unwords ["Unknown function:", needle]
        Just s -> putStrLn $ pretty s


searchx (Search needle numresults threshold _) db = do
    let numresults' = if numresults <= 0 then maxBound else numresults

    forM_ (take numresults' $ search db needle threshold) $
        putStrLn . pretty . snd

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


writeDb :: Maybe String -> DB -> IO ()
writeDb p db = do
    datadir <- getDbPath p
    createDirectoryIfMissing True datadir
    encodeFile (datadir </> "jassbot.db") db

mkDatabasex cj bj p = do
    c <- parse programm "common.j" <$> readFile cj
    b <- parse programm "Blizzard.j" <$> readFile bj

    case (c, b) of
        (Right (Programm c'), Right (Programm b')) -> 
            writeDb p . buildDatabase . Programm $ c' <> b'

        (Left err, _) -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitWith $ ExitFailure 2
        (_, Left err) -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitWith $ ExitFailure 2





