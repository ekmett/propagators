{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
module Model.Internal.Counted where

import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Control.Monad.Fix
import Data.Foldable
import GHC.Exts as Exts
import Prelude

-- | A simple list with a baked in memoization of the length
data Counted a = Counted {-# UNPACK #-} !Int [a] deriving (Functor,Traversable,Eq,Ord,Read,Show)

pattern (:+) :: () => () => a -> Counted a -> Counted a
pattern a :+ as <- Counted (subtract 1 -> i) (a : (Counted i -> as)) where
  a :+ Counted i as = Counted (i+1) (a:as)

instance Foldable Counted where
  foldMap f (Counted _ xs) = foldMap f xs
  foldr f z (Counted _ xs) = foldr f z xs
  length (Counted i _) = i
  null (Counted i _) = i == 0
  toList (Counted _ xs) = xs

instance Exts.IsList (Counted a) where
  type Item (Counted a) = a
  fromListN n xs = Counted n xs
  fromList xs = Counted (length xs) xs
  toList (Counted _ xs) = xs

instance Applicative Counted where
  pure a = Counted 1 [a]
  Counted n fs <*> Counted m as = Counted (n*m) (fs <*> as)

instance Monad Counted where
  return a = Counted 1 [a]
  fail _ = Counted 0 []
  Counted _ as >>= f = Counted (sum (length <$> bs)) (bs >>= Exts.toList) -- one of these days i should try Twan's trick here
    where bs = fmap f as

instance Alternative Counted where
  empty = Counted 0 []
  Counted n as <|> Counted m bs = Counted (n + m) (as ++ bs)

instance MonadPlus Counted where
  mzero = empty
  mplus = (<|>)

instance MonadZip Counted where
  mzipWith f (Counted n as) (Counted m bs) = Counted (min n m) (mzipWith f as bs)
  munzip (Counted n as) = case munzip as of
    (bs,cs) -> (Counted n bs, Counted n cs)

instance MonadFix Counted where
  mfix f = case fix (f . head . Exts.toList) of
    Counted _ [] -> Counted 0 []
    Counted n (x:_) -> Counted n (x : mfix (tail . Exts.toList . f))
