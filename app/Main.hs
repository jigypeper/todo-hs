{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Data.Time
import           System.IO.Error
import           Data.Aeson hiding (Options)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           Options.Applicative hiding (infoParser)
import           Data.List.Safe ((!!))
import           Prelude hiding ((!!))
import           Data.String.Utils
import           System.Directory
import           Control.Monad

type ItemIndex = Int
type ItemDescription = Maybe String
type ItemTitle = String
type ItemPriority = Maybe Priority
type ItemDueBy = Maybe LocalTime

data Priority = Low | Normal | High deriving (Generic, Show)
instance ToJSON Priority
instance FromJSON Priority

data ToDoList = ToDoList [Item] deriving (Generic, Show)
instance ToJSON ToDoList
instance FromJSON ToDoList

data Item = Item
    { title :: ItemTitle
    , description :: ItemDescription
    , priority :: ItemPriority
    , dueBy :: ItemDueBy  
    } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item
    
data ItemUpdate = ItemUpdate
    { titleUpdate :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate :: Maybe ItemPriority
    , dueByUpdate :: Maybe ItemDueBy  
    } deriving Show

data Options = Options FilePath Command deriving Show

data Command =
    Info
    | Init
    | List
    | Add Item
    | View ItemIndex
    | Update ItemIndex ItemUpdate
    | Remove ItemIndex
    deriving Show

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

defaultDataPath :: FilePath
defaultDataPath = "~/.todo.yaml"

optionsParser :: Parser Options
optionsParser = Options
    <$> dataPathParser
    <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long "data-path"
    <> short 'p'
    <> metavar "DATAPATH"
    <> help ("path to data file (default " ++ defaultDataPath ++ ")")

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

main :: IO ()
main = do
    Options dataPath command <- execParser (info (optionsParser) (progDesc "To-do list"))
    -- toDoList <- readToDoList dataPath
    -- print toDoList
    homeDir <- getHomeDirectory
    let expandedDataPath = replace "~" homeDir dataPath
    run expandedDataPath command

run :: FilePath -> Command -> IO ()
run dataPath Info = showInfo dataPath
run dataPath Init = initItems dataPath
run dataPath List = viewItems dataPath
run dataPath (Add item) = addItem dataPath item
run dataPath (View idx) = viewItem dataPath idx
run dataPath (Update idx itemUpdate) = updateItem dataPath idx itemUpdate
run dataPath (Remove idx) = removeItem dataPath idx

writeToDoList :: FilePath -> ToDoList -> IO ()
writeToDoList dataPath toDoList = BS.writeFile dataPath (Yaml.encode toDoList)

updateItem :: FilePath -> ItemIndex -> ItemUpdate -> IO ()
updateItem dataPath idx (ItemUpdate mbTitle mbDescription mbPriority mbDueBy) = do
    ToDoList items <- readToDoList dataPath
    let update (Item title description priority dueBy) = Item
            (updateField mbTitle title)
            (updateField mbDescription description)
            (updateField mbPriority priority)
            (updateField mbDueBy dueBy)
        updateField (Just value) _ = value
        updateField Nothing value = value
        mbItems = updateAt items idx update
    case mbItems of
      Nothing -> putStrLn "Invalid item index"
      Just items' -> do
          let toDoList = ToDoList items'
          writeToDoList dataPath toDoList

updateAt :: [a] -> Int -> (a -> a) -> Maybe [a]
updateAt xs idx f =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, after) = splitAt idx xs
            element : after' = after
            xs' = before ++ [f element] ++ after'
        in Just xs'

removeAt :: [a] -> Int -> Maybe [a]
removeAt xs idx =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, after) = splitAt idx xs
            _ : after' = after
            xs' = before ++ after'
        in Just xs'

readToDoList :: FilePath -> IO ToDoList
readToDoList dataPath = do
    mbToDoList <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (BS.readFile dataPath >>= return . Yaml.decode)
        (\_ -> return $ Just (ToDoList []))
    case mbToDoList of
        Nothing -> error "Yaml file is corrupt"
        Just toDoList -> return toDoList

initItems :: FilePath -> IO ()
initItems dataPath = writeToDoList dataPath (ToDoList [])

showInfo :: FilePath -> IO ()
showInfo dataPath = do
    putStrLn $ "Data file path: " ++ dataPath
    exists <- doesFileExist dataPath
    if exists
    then do
        s <- BS.readFile dataPath
        let mbToDoList = Yaml.decode s
        case mbToDoList of
            Nothing -> putStrLn $ "Status: file is invalid"
            Just (ToDoList items) -> putStrLn $ "Status: contains " ++ show (length items) ++ " items"
    else putStrLn $ "Status: file does not exist"

viewItems :: FilePath -> IO ()
viewItems dataPath = do
        ToDoList items <- readToDoList dataPath
        forM_
            (zip [0..] items)
            (\(idx, item) -> showItem idx item)

showItem :: ItemIndex -> Item -> IO ()
showItem idx (Item title mbDescription mbPriority mbDueBy) = do
    putStrLn $ "[" ++ show idx ++ "]: " ++ title
    putStr " Description: "
    putStrLn $ showField id mbDescription
    putStr " Priority: "
    putStrLn $ showField show mbPriority
    putStr " Due by: "
    putStrLn $ showField (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S") mbDueBy

showField :: (a -> String) -> Maybe a -> String
showField f (Just x) = f x
showField _ Nothing = "(not set)"

addItem :: FilePath -> Item -> IO ()
addItem dataPath item = do
    ToDoList items <- readToDoList dataPath
    let newToDoList = ToDoList (item : items)
    writeToDoList dataPath newToDoList

removeItem :: FilePath -> ItemIndex -> IO ()
removeItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItems = items `removeAt` idx
    case mbItems of
        Nothing -> putStrLn "Invalid item index"
        Just items' -> do
            let toDoList = ToDoList items'
            writeToDoList dataPath toDoList

viewItem :: FilePath -> ItemIndex -> IO ()
viewItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItem = items !! idx
    case mbItem of
        Nothing -> putStrLn "Invalid item index"
        Just item -> showItem idx item
