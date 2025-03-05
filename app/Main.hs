{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Todo.Types
import           Todo.Parser
import           Todo.Actions
import           Todo.Util
import           System.Directory

main :: IO ()
main = do
    Options dataPath command <- execParser (info (optionsParser) (progDesc "To-do list"))
    expandedDataPath <- expandHomePath dataPath
    run expandedDataPath command

run :: FilePath -> Command -> IO ()
run dataPath Info = showInfo dataPath
run dataPath Init = initItems dataPath
run dataPath List = viewItems dataPath
run dataPath (Add item) = addItem dataPath item
run dataPath (View idx) = viewItem dataPath idx
run dataPath (Update idx itemUpdate) = updateItem dataPath idx itemUpdate
run dataPath (Remove idx) = removeItem dataPath idx
