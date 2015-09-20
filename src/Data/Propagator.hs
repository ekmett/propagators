module Data.Propagator
  ( Cell(..)
  , Change(..)
  , Merging(..)
  , cell
  , cellWith
  , known
  , write, require, with
  , watch
  , watch2
  , lift1, lift2
  , Prop
  , nullary
  , unary
  , binary
  , lower, arg
  , lower1, lower2
  , forward
  , backward
  ) where

import Data.Propagator.Internal
