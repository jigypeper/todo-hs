{-# LANGUAGE OverloadedStrings #-}

module Todo.Actions where

import           Control.Exception
import           Control.Monad
import           Data.List.Safe ((!!))
import           Prelude hiding ((!!))
import           System.Directory
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import           Data.String.Utils
import           Data.Time
import           Todo.Types
import           Todo.Util
import           System.IO.Error (isDoesNotExistError)

-- Write a todo list to a YAML file
writeToDoList :: FilePath -> ToDoList -> IO ()
writeToDoList dataPath toDoList = BS.writeFile dataPath (Yaml.encode toDoList)

-- Read todo list from a YAML file
readToDoList :: FilePath -> IO ToDoList
readToDoList dataPath = do
    mbToDoList <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (BS.readFile dataPath >>= return . Yaml.decode)
        (\_ -> return $ Just (ToDoList []))
    case mbToDoList of
        Nothing -> error "Yaml file is corrupt"
        Just toDoList -> return toDoList

-- Initialize an empty todo list
initItems :: FilePath -> IO ()
initItems dataPath = writeToDoList dataPath (ToDoList [])

-- Show information about the todo list file
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

-- View all items in the todo list
viewItems :: FilePath -> IO ()
viewItems dataPath = do
        ToDoList items <- readToDoList dataPath
        forM_
            (zip [0..] items)
            (\(idx, item) -> showItem idx item)

-- Show a single item with its details
showItem :: ItemIndex -> Item -> IO ()
showItem idx (Item title mbDescription mbPriority mbDueBy) = do
    putStrLn $ "[" ++ show idx ++ "]: " ++ title
    putStr " Description: "
    putStrLn $ showField id mbDescription
    putStr " Priority: "
    putStrLn $ showField show mbPriority
    putStr " Due by: "
    putStrLn $ showField (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S") mbDueBy

-- Add a new item to the todo list
addItem :: FilePath -> Item -> IO ()
addItem dataPath item = do
    ToDoList items <- readToDoList dataPath
    let newToDoList = ToDoList (item : items)
    writeToDoList dataPath newToDoList

-- Update an existing item in the todo list
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

-- Remove an item from the todo list
removeItem :: FilePath -> ItemIndex -> IO ()
removeItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItems = items `removeAt` idx
    case mbItems of
        Nothing -> putStrLn "Invalid item index"
        Just items' -> do
            let toDoList = ToDoList items'
            writeToDoList dataPath toDoList

-- View a specific item
viewItem :: FilePath -> ItemIndex -> IO ()
viewItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItem = items !! idx
    case mbItem of
        Nothing -> putStrLn "Invalid item index"
        Just item -> showItem idx item
