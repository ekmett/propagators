{-# LANGUAGE DeriveAnyClass #-}
module Model.Exception where

import Control.Exception

data BlockedIndefinitelyOnIVar = BlockedIndefinitelyOnIVar deriving (Show,Exception)

data Contradiction = Contradiction deriving (Show,Exception)
