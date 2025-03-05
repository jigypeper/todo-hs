{-# LANGUAGE OverloadedStrings #-}

module Todo.Parser where

import           Options.Applicative hiding (infoParser)
import           Data.Time
import           Todo.Types

-- Default data file path
defaultDataPath :: FilePath
defaultDataPath = "~/.todo.yaml"

-- Parse command-line options
optionsParser :: Parser Options
optionsParser = Options
    <$> dataPathParser
    <*> commandParser

-- Parser for data file path
dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long "data-path"
    <> short 'p'
    <> metavar "DATAPATH"
    <> help ("path to data file (default " ++ defaultDataPath ++ ")")

-- Command-specific parsers
commandParser :: Parser Command
commandParser = subparser $ mconcat
    [ command "info" (info infoParser (progDesc "Show info"))
    , command "init" (info initParser (progDesc "Initialize items"))
    , command "list" (info listParser (progDesc "List all items"))
    , command "add" (info addParser (progDesc "Add item"))
    , command "view" (info viewParser (progDesc "View item"))
    , command "update" (info updateParser (progDesc "Update item"))
    , command "remove" (info removeParser (progDesc "Remove item"))
    ]

-- Specific command parsers
infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> addItemParser

addItemParser :: Parser Item
addItemParser = Item
    <$> argument str (metavar "TITLE" <> help "title")
    <*> optional itemDescriptionValueParser 
    <*> optional itemPriorityValueParser 
    <*> optional itemDueByValueParser
    
viewParser :: Parser Command
viewParser = View <$> itemIndexParser

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
    <$> optional updateItemTitleParser
    <*> optional updateItemDescriptionParser 
    <*> optional updateItemPriorityParser 
    <*> optional updateItemDueByParser

-- Specific field parsers
updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
    Just <$> itemDescriptionValueParser
    <|> flag' Nothing (long "clear-desc")
    
updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser =
    Just <$> itemPriorityValueParser
    <|> flag' Nothing (long "clear-priority")

updateItemDueByParser :: Parser ItemDueBy
updateItemDueByParser =
    Just <$> itemDueByValueParser
    <|> flag' Nothing (long "clear-due-by")

removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

-- Helper parsers for input fields
itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

itemTitleValueParser :: Parser String
itemTitleValueParser = 
        strOption (long "title" <> short 't' <> metavar "TITLE" <> help "title")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
    strOption (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")
    
itemPriorityValueParser :: Parser Priority
itemPriorityValueParser =
    option readPriority (long "priority" <> short 'p' <> metavar "PRIORITY" <> help "priority")
        where
            readPriority = eitherReader $ \arg ->
                case arg of
                    "1" -> Right Low
                    "2" -> Right Normal
                    "3" -> Right High
                    _ -> Left $ "Invalid priority value" ++ arg

itemDueByValueParser :: Parser LocalTime
itemDueByValueParser =
    option readDateTime (long "due-by" <> short 'd' <> metavar "DUE-BY" <> help "due by")
    where
        readDateTime = eitherReader $ \arg ->
            case parseDateTimeMaybe arg of
                (Just dateTime) -> Right dateTime
                Nothing -> Left $ "Date/time string must be in" ++ dateTimeFormat ++ " format"
        parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
        dateTimeFormat = "%Y/%m/%d %H:%M:%S"
