{-# LANGUAGE OverloadedStrings #-}

module TodoSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Todo.Types
import           Todo.Util
import           Data.Time
import           System.Random (Random)

spec :: Spec
spec = do
    describe "Todo.Util" $ do
        describe "updateAt" $ do
            it "updates an item at a valid index" $ do
                let list = [1, 2, 3, 4]
                updateAt list 1 (*10) `shouldBe` Just [1, 20, 3, 4]
            
            it "returns Nothing for an invalid index" $ do
                let list = [1, 2, 3]
                updateAt list 3 (*10) `shouldBe` Nothing
                updateAt list (-1) (*10) `shouldBe` Nothing

        describe "removeAt" $ do
            it "removes an item at a valid index" $ do
                let list = [1, 2, 3, 4]
                removeAt list 1 `shouldBe` Just [1, 3, 4]
            
            it "returns Nothing for an invalid index" $ do
                let list = [1, 2, 3]
                removeAt list 3 `shouldBe` Nothing
                removeAt list (-1) `shouldBe` Nothing

    describe "Todo.Types" $ do
        describe "Todo Item" $ do
            it "can create an item with all fields" $ do
                let now = read "2024-03-05 12:00:00" :: LocalTime
                let item = Item 
                        "Test Title" 
                        (Just "Test Description") 
                        (Just High) 
                        (Just now)
                title item `shouldBe` "Test Title"
                description item `shouldBe` Just "Test Description"
                priority item `shouldBe` Just High
                dueBy item `shouldBe` Just now

            it "can create an item with minimal fields" $ do
                let item = Item 
                        "Minimal Title" 
                        Nothing 
                        Nothing 
                        Nothing
                title item `shouldBe` "Minimal Title"
                description item `shouldBe` Nothing
                priority item `shouldBe` Nothing
                dueBy item `shouldBe` Nothing

        describe "Priority" $ do
            it "can convert priorities" $ do
                show Low `shouldBe` "Low"
                show Normal `shouldBe` "Normal"
                show High `shouldBe` "High"

    describe "ItemUpdate" $ do
        it "can create an update with all fields" $ do
            let now = read "2024-03-05 12:00:00" :: LocalTime
            let update = ItemUpdate 
                    (Just "New Title") 
                    (Just $ Just "New Description")
                    (Just $ Just High)
                    (Just $ Just now)
            titleUpdate update `shouldBe` Just "New Title"
            descriptionUpdate update `shouldBe` Just (Just "New Description")
            priorityUpdate update `shouldBe` Just (Just High)
            dueByUpdate update `shouldBe` Just (Just now)

        it "can create an update with minimal fields" $ do
            let update = ItemUpdate 
                    Nothing 
                    Nothing
                    Nothing
                    Nothing
            titleUpdate update `shouldBe` Nothing
            descriptionUpdate update `shouldBe` Nothing
            priorityUpdate update `shouldBe` Nothing
            dueByUpdate update `shouldBe` Nothing

-- Arbitrary instances for QuickCheck
instance Arbitrary Priority where
    arbitrary = elements [Low, Normal, High]

-- Explicit Random and Arbitrary instances for Day
instance Arbitrary Day where
    arbitrary = do
        y <- choose (2020, 2030) :: Gen Int
        m <- choose (1, 12) :: Gen Int
        d <- choose (1, 28) :: Gen Int
        return $ fromGregorian (toInteger y) m d

-- Explicit Random and Arbitrary instances for TimeOfDay
instance Arbitrary TimeOfDay where
    arbitrary = do
        h <- choose (0, 23) :: Gen Int
        m <- choose (0, 59) :: Gen Int
        s <- choose (0, 59) :: Gen Int
        return $ TimeOfDay h m (fromIntegral s)

-- Explicit Arbitrary instance for LocalTime
instance Arbitrary LocalTime where
    arbitrary = do
        day <- arbitrary
        timeOfDay <- arbitrary
        return $ LocalTime day timeOfDay

-- Arbitrary instance for Item with more robust generation
instance Arbitrary Item where
    arbitrary = do
        title <- listOf1 (elements ['a'..'z']) `suchThat` (not . null)
        desc <- oneof [return Nothing, Just <$> listOf1 (elements ['a'..'z']) `suchThat` (not . null)]
        prio <- oneof [return Nothing, Just <$> arbitrary]
        dueBy <- oneof [return Nothing, Just <$> arbitrary]
        return $ Item title desc prio dueBy

-- Property tests
prop_item_roundtrip :: Item -> Bool
prop_item_roundtrip item = 
    title item == title item &&
    description item == description item &&
    priority item == priority item &&
    dueBy item == dueBy item
