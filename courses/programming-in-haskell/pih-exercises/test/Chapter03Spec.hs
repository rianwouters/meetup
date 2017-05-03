-- | Specification for the exercises of Chapter 03.

module Chapter03Spec where

import           Chapter03       (double, pair, palindrome, second, swap, t0,
                                  t1, twice, xs0, xs1, xs2, xs3)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck (Property, property, (.&.), (===), (==>))

spec :: Spec
spec = do
  describe "Exercise 1" $ do

    it "implements xs0" $ do
      xs0 `shouldBe` [0, 1]

    it "implements xs1" $ do
      xs1 `shouldBe` ['a','b','c']

    it "implements xs2" $ do
      xs2 `shouldBe` [(False,'0'),(True,'1')]

    it "implements xs3" $ property $ \xs ->
          (not (null xs) ==> (xs3 !! 0) xs === tail (xs :: [Int]))
      .&. (not (null xs) ==> (xs3 !! 1) xs === init xs)
      .&. (xs3 !! 2) xs === reverse xs

    it "implements t0" $ do
      t0 `shouldBe` ('a','b','c')

    it "implements t1" $ do
      t1 `shouldBe` ([False,True],['0','1'])

  describe "Exercise 2" $ do

    it "implements function second" $ do
      second [1,2,3,4] `shouldBe` 2
      -- | intentionally not testing second [] and second [1] as it is not specified

    it "implements function swap" $ do
      swap (0,0) `shouldBe` (0,0)
      swap ((1,2)::(Int,Int)) `shouldBe` (2,1)
      swap (1,'a') `shouldBe` ('a',1)

    it "implements function pair" $ do
      pair 0 0 `shouldBe` (0,0)
      pair 1 2 `shouldBe` (1,2)
      pair 1 'a' `shouldBe` (1,'a')

    it "implements function double" $ do
      double 0 `shouldBe` 0
      double 3.14 `shouldBe` 6.28
      double (-2.54) `shouldBe` (-5.08)

    it "implements function palindrome" $ do
      palindrome ([]::[Int]) `shouldBe` True
      palindrome "parterretrapje" `shouldBe` False
      palindrome "parterretrap" `shouldBe` True

    it "implements function twice" $ do
      twice (+3) 8 `shouldBe` (8 + 3 + 3)
      twice (++" bar") "foo" `shouldBe` "foo bar bar"
