module Main (main) where

import Test.Hspec
import TodoSpec (spec, prop_item_roundtrip)
import Test.QuickCheck

main :: IO ()
main = do
    hspec spec
    quickCheck prop_item_roundtrip
