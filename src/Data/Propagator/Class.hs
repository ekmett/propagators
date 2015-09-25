{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Propagator.Class
  ( Change(..)
  , Propagated(..)
  , mergeDefault
  ) where

import Control.Applicative
import Control.Monad
import Data.HashSet
import Data.Propagator.Name
import Numeric.Interval.Internal (Interval(..))
import Numeric.Natural

-- | This represents the sorts of changes we can make as we accumulate information
-- in a 'Data.Propagator.Cell.Cell'.
--
-- * 'Change' 'False' indicates that this is the old value, and we didn't change anything.
--
-- * 'Change' 'True' indicates that this is the new value, which gains information over the old.
--
-- * 'Contradiction' indicates that the updated information is inconsistent with the old.
data Change a
  = Change !Bool a
  | Contradiction !(HashSet Name) String
  deriving (Functor, Foldable, Traversable)

instance Applicative Change where
  pure = Change False
  Change m f <*> Change n a = Change (m || n) (f a)
  Contradiction m n <*> _ = Contradiction m n
  _ <*> Contradiction m n = Contradiction m n

instance Alternative Change where
  empty = Contradiction mempty "contradiction"
  Contradiction{} <|> n = n
  m               <|> _ = m
  -- can we (evilly) intersect the contradiction set?

instance Monad Change where
  return = Change False
  Change m a >>= f = case f a of
    Change n b -> Change (m || n) b
    Contradiction s n -> Contradiction s n
  Contradiction s n >>= _ = Contradiction s n
  fail = Contradiction mempty

instance MonadPlus Change where
  mzero = Control.Applicative.empty
  mplus = (<|>)

-- | This is a viable default definition for 'merge' for most simple values.
mergeDefault :: (Eq a, Show a) => a -> a -> Change a
mergeDefault a b
  | a == b    = Change False a
  | otherwise = Contradiction mempty $ (showString "merge: " . showsPrec 10 a . showString " /= " . showsPrec 10 b) ""

-- | This class provides the default definition for how to 'merge' values in our information lattice.
--
class Propagated a where
  -- | The first argument represents the old information. The second argument represents the new information.
  -- When the new information causes us to learn something this should return a @'Change' 'True'@ with the combined
  -- information. When it doesn't, it should return @'Change' 'False'@ with the old information. If new information
  -- is inconsistent with the old, it should return 'Contradiction' instead.
  merge :: a -> a -> Change a
  default merge :: (Eq a, Show a) => a -> a -> Change a
  merge = mergeDefault

instance Propagated ()
instance Propagated Bool
instance Propagated Int
instance Propagated Integer
instance Propagated Word
instance Propagated Rational
instance Propagated Natural

-- | Approximate equality (1e-6)
instance Propagated Float where
  merge a b
    | isNaN a && isNaN b                     = Change False a
    | isInfinite a && isInfinite b && a == b = Change False a
    | abs (a-b) < 1e-6                       = Change False a
    | otherwise = Contradiction mempty $ (showString "merge: " . showsPrec 10 a . showString " /= " . showsPrec 10 b) ""

-- | Approximate equality (1e-9)
instance Propagated Double where
  merge a b
    | isNaN a && isNaN b                     = Change False a
    | isInfinite a && isInfinite b && a == b = Change False a
    | abs (a-b) < 1e-9                       = Change False a
    | otherwise = Contradiction mempty $ (showString "merge: " . showsPrec 10 a . showString " /= " . showsPrec 10 b) ""

instance (Propagated a, Propagated b) => Propagated (a, b) where
  merge (a,b) (c,d) = (,) <$> merge a c <*> merge b d

instance (Propagated a, Propagated b) => Propagated (Either a b) where
  merge (Left a)  (Left b)  = Left <$> merge a b
  merge (Right a) (Right b) = Right <$> merge a b
  merge _ _ = fail "Left /= Right"

-- | Propagated interval arithmetic
instance (Num a, Ord a) => Propagated (Interval a) where
  merge (I a b) (I c d)
    | b < c || d < a = Change True Empty
    | otherwise      = Change (a < c || b > d) $ I (max a c) (min b d)
  merge Empty _ = Change False Empty
  merge _ Empty = Change True Empty
