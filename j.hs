{-# LANGUAGE GADTs #-}

import Options.Applicative

import Control.Applicative
import Control.Monad

import Data.Binary (decodeFile, encodeFile)

import Text.Megaparsec (parse, errorBundlePretty)
import Jass.Parser
import Jass.Ast

import Jassbot.Signature (pretty)
import Jassbot.Search (search)
import Jassbot.Typeof (typeof)
import Jassbot.DB (buildDatabase)


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

main = do
    options <- parseOptions
    case options of
        TypeOf needle -> typeOfx needle
        Search needle -> searchx needle
        MkDatabase cj bj -> mkDatabasex cj bj

typeOfx needle = do
    db <- decodeFile "jassbot.db"
    let x = typeof db needle
    case x of
        Nothing -> putStrLn "Unknown function"
        Just s -> putStrLn $ pretty s


searchx needle = do
    db <- decodeFile "jassbot.db"

    forM_ (take 3 $ search db needle) $
        putStrLn . pretty . snd

mkDatabasex cj bj = do
    c <- parse programm "common.j" <$> readFile cj
    b <- parse programm "Blizzard.j" <$> readFile bj

    case (c, b) of
        (Right (Programm c'), Right (Programm b')) -> do
            encodeFile "jassbot.db" $ buildDatabase $ Programm (c' <> b')





