{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.Types where

import           Data.Aeson
import           Data.Time
import           GHC.Generics

-- Type aliases for clarity
type ItemIndex = Int
type ItemDescription = Maybe String
type ItemTitle = String
type ItemPriority = Maybe Priority
type ItemDueBy = Maybe LocalTime

-- Priority levels for todo items
data Priority = Low | Normal | High 
    deriving (Generic, Show, Eq)

instance ToJSON Priority
instance FromJSON Priority

-- Main todo list data structure
data ToDoList = ToDoList [Item] 
    deriving (Generic, Show, Eq)

instance ToJSON ToDoList
instance FromJSON ToDoList

-- Individual todo item
data Item = Item
    { title :: ItemTitle
    , description :: ItemDescription
    , priority :: ItemPriority
    , dueBy :: ItemDueBy  
    } deriving (Generic, Show, Eq)

instance ToJSON Item
instance FromJSON Item

-- Structure for updating an existing item
data ItemUpdate = ItemUpdate
    { titleUpdate :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate :: Maybe ItemPriority
    , dueByUpdate :: Maybe ItemDueBy  
    } deriving (Show, Eq)

-- Supported commands for the todo application
data Command =
    Info
    | Init
    | List
    | Add Item
    | View ItemIndex
    | Update ItemIndex ItemUpdate
    | Remove ItemIndex
    deriving (Show, Eq)

-- Application options
data Options = Options FilePath Command 
    deriving (Show, Eq)
