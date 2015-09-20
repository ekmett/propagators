module Data.Propagator
  ( Cell
  , Change(..)
  , Propagated(..)
  , PropagatedNum
  , PropagatedFloating
  , PropagatedInterval
  , cell
  , cellWith
  , known
  , write, require, with
  , watch
  , watch2
  , lift1, lift2
  , Prop(..)
  , lower, arg
  , lower1, lower2
  , forwards
  , backwards
  ) where

import Data.Propagator.Cell
import Data.Propagator.Class
import Data.Propagator.Num
import Data.Propagator.Prop
