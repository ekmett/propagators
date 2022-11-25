{-# LANGUAGE ScopedTypeVariables #-}

module Data.Propagator.Tests (tests) where

import Data.Propagator
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

tests =
  testGroup
    "Unit tests"
    [testForwards, testBackwards]

testForwards = testCase "forwards" $ assertEqual [] (forwards (\c -> c * 9/5 + 32) 100) (Just (212 :: Float))

testBackwards = testCase "backwards" $ assertEqual [] (backwards (\c -> c * 9/5 + 32) 212) (Just (100 :: Float))