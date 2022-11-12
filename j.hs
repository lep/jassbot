{-# LANGUAGE GADTs #-}

import Options.Applicative

import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

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

exceptT :: Either e a -> ExceptT e IO a
exceptT = ExceptT . return

data Command = TypeOf String (Maybe String)
             | Search String Int Double (Maybe String)
             | MkDatabase [FilePath] (Maybe FilePath)

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
                               <> help "Display at most N results. Use 0 to disable the limit."
                               <> metavar "N"
                               )
               <*> option auto ( showDefault 
                               <> value 0.4
                               <> long "threshold"
                               <> short 't'
                               <> help "Minimum score for a function to be displayed"
                               <> metavar "T"
                               )
               <*> optional (option str $ long "data-dir")

    mkDatabaseOptions =
        MkDatabase <$> some (argument str (help "path to jass files to idnex" <> metavar "[JASSFILES]"))
                   <*> optional (option str $ long "data-dir")


main = do
    options <- parseOptions
    case options of
        TypeOf needle p -> readDb p >>= typeOfx needle
        Search _ _ _ p -> readDb p >>= searchx options
        MkDatabase jassFiles databasePath -> mkDatabasex jassFiles databasePath

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



mkDatabasex :: [FilePath] -> Maybe FilePath -> IO ()
mkDatabasex jassFiles databasePath = do
    x <- runExceptT $ forM jassFiles $ \path -> do
        src <- liftIO $ readFile path
        exceptT $ parse programm path src
    case x of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitWith $ ExitFailure 2
        Right progs -> do
            writeDb databasePath . buildDatabase $ concatPrograms progs
  where
    getToplevel :: Ast x Programm -> [Ast x Toplevel]
    getToplevel (Programm xs) = xs

    concatPrograms :: [Ast x Programm] -> Ast x Programm
    concatPrograms = Programm . concatMap getToplevel
