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
import Numeric.Natural

data Change a 
  = Change !Bool a
  | Contradiction String
  deriving (Functor, Foldable, Traversable)

instance Applicative Change where
  pure = Change False
  Change m f <*> Change n a = Change (m || n) (f a)
  Contradiction m <*> _ = Contradiction m
  _ <*> Contradiction m = Contradiction m

instance Alternative Change where
  empty = Contradiction "empty"
  Contradiction{} <|> n = n
  m               <|> _ = m

instance Monad Change where
  return = Change False
  Change m a >>= f = case f a of
    Change n b -> Change (m || n) b
    Contradiction s -> Contradiction s
  Contradiction s >>= _ = Contradiction s
  fail = Contradiction

instance MonadPlus Change where
  mzero = empty
  mplus = (<|>)
  
mergeDefault :: (Eq a, Show a) => a -> a -> Change a
mergeDefault a b
  | a == b    = Change False a
  | otherwise = Contradiction $ (showString "merge: " . showsPrec 10 a . showString " /= " . showsPrec 10 b) ""

class Propagated a where
  merge :: a -> a -> Change a
  default merge :: (Eq a, Show a) => a -> a -> Change a
  merge = mergeDefault

instance Propagated Int
instance Propagated Integer
instance Propagated Word
instance Propagated Rational
instance Propagated Natural
instance Propagated Float -- pretty brittle!
instance Propagated Double -- pretty brittle!

instance (Propagated a, Propagated b) => Propagated (a, b) where
  merge (a,b) (c,d) = (,) <$> merge a c <*> merge b d

instance (Propagated a, Propagated b) => Propagated (Either a b) where
  merge (Left a)  (Left b)  = Left <$> merge a b
  merge (Right a) (Right b) = Right <$> merge a b
  merge _ _ = fail "Left /= Right"
