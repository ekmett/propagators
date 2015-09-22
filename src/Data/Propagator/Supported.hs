{-# LANGUAGE DeriveTraversable #-}
module Data.Propagator.Supported where

import Control.Applicative
import Data.HashSet
import Data.Propagator.Class

data Supported a = Supported !(HashSet String) a
  deriving (Functor, Foldable, Traversable, Read, Show)

instance Eq a => Eq (Supported a) where
  Supported _ a == Supported _ b = a == b

instance Ord a => Ord (Supported a) where
  Supported _ a `compare` Supported _ b = compare a b

instance Applicative Supported where
  pure = Supported mempty
  Supported xs a <*  Supported ys _ = Supported (union xs ys) a
  Supported xs _  *> Supported ys b = Supported (union xs ys) b
  Supported xs f <*> Supported ys a = Supported (union xs ys) (f a)

instance Monad Supported where
  return = Supported mempty
  (>>) = (*>)
  Supported xs a >>= f = case f a of
    Supported ys b -> Supported (union xs ys) b

instance Propagated a => Propagated (Supported a) where
  merge (Supported xs a) (Supported ys b) = case merge a b of
    Change False c     -> Change False (Supported xs c)
    Change True c      -> Change True  (Supported (union xs ys) c)
    Contradiction zs s -> Contradiction (zs `union` xs `union` ys) s

instance Num a => Num (Supported a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger
  
instance Fractional a => Fractional (Supported a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating a => Floating (Supported a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  logBase = liftA2 logBase
  (**) = liftA2 (**)
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
