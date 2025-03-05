module Todo.Util where

import           Data.Time
import           System.Directory

-- Helper function to show Maybe fields
showField :: (a -> String) -> Maybe a -> String
showField f (Just x) = f x
showField _ Nothing = "(not set)"

-- Safely update an item in a list at a specific index
updateAt :: [a] -> Int -> (a -> a) -> Maybe [a]
updateAt xs idx f =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, after) = splitAt idx xs
            element : after' = after
            xs' = before ++ [f element] ++ after'
        in Just xs'

-- Safely remove an item from a list at a specific index
removeAt :: [a] -> Int -> Maybe [a]
removeAt xs idx =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, after) = splitAt idx xs
            _ : after' = after
            xs' = before ++ after'
        in Just xs'

-- Expand home directory in file paths (e.g., replace "~" with actual home directory)
expandHomePath :: FilePath -> IO FilePath
expandHomePath path = do
    homeDir <- getHomeDirectory
    return $ replaceHome path
  where
    replaceHome p
      | "~/" `isPrefixOf` p = homeDir ++ drop 2 p
      | "~" == p = homeDir
      | otherwise = p
    
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    homeDir = "/home/user"  -- This will be replaced by getHomeDirectory
