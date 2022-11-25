module Main where

import Test.Framework (defaultMain)

import qualified Data.Propagator.Tests

main :: IO ()
main = defaultMain
    [
        Data.Propagator.Tests.tests
    ]