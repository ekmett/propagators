-- | Inspired by the names in nominal adapton.
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
module Data.Propagator.Name 
  ( Name
  , fresh
  , fork
  , pair
  , child
  , children
  , height
  ) where

import Control.Monad.Primitive
import Data.Bits
import Data.Hashable
import Data.String
import GHC.Prim
import GHC.Types

data Path
  = U Addr# (MutableByteArray# RealWorld)
  | S String
  | P !Path !Path
  | C {-# UNPACK #-} !Int Path

instance Eq Path where
  U _ i == U _ j = isTrue# (sameMutableByteArray# i j)
  S s   == S t   = s == t
  P p q == P r s = p == r && q == s
  C m p == C n q = m == n && p == q
  _ == _ = False

instance Show Path where
  showsPrec d (U a _) = showParen (d > 10) $ showString "unique {" . showsPrec 11 (I# (addr2Int# a)) . showChar '}'
  showsPrec d (S s) = showsPrec d s
  showsPrec d (P l r) = showParen (d > 10) $ showString "P " . showsPrec 11 l . showChar ' ' . showsPrec 11 r
  showsPrec d (C n p) = showParen (d > 10) $ showString "C " . showsPrec 11 n . showChar ' ' . showsPrec 11 p

data Name = Name
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  !Path
  deriving Show

height :: Name -> Int
height (Name _ h _) = h
{-# INLINE height #-}

instance Eq Name where
  Name i _ p == Name j _ q = i == j && p == q
  {-# INLINE (==) #-}

instance Hashable Name where
  hashWithSalt d (Name h _ _) = hashWithSalt d h
  {-# INLINE hashWithSalt #-}

ffs :: Int -> Int
ffs 0 = 0
ffs x = countTrailingZeros x + 1
{-# INLINE ffs #-}

instance IsString Name where
  fromString s = name (hash s) (S s)
  {-# INLINE fromString #-}

name :: Int -> Path -> Name
name h p = Name h (ffs h) p
{-# INLINE name #-}

-- | gensym. this scheme avoids the unique barrier.
fresh :: PrimMonad m => m Name
fresh = unsafePrimToPrim $ IO $ \s -> case newByteArray# 0# s of
  (# s', ba #) -> case unsafeCoerce# ba of
    n# -> (# s', name (I# (addr2Int# n#)) (U n# ba) #)
{-# INLINE fresh #-}

-- | obtain the name of two children deterministically.
fork :: Name -> (Name, Name)
fork n = (child 1 n, child 2 n)
{-# INLINE fork #-}

-- | obtain the name of the kth child (starting from 1)
child :: Int -> Name -> Name
child d (Name i h p) = Name (hashWithSalt d i) h (C d p)
{-# INLINE child #-}

-- | obtain the names of all children
children :: Name -> [Name]
children n = (`child` n) <$> [1..]
{-# INLINE children #-}

-- | build a name based on two existing names
pair :: Name -> Name -> Name
pair (Name i h p) (Name j h' q) = Name (hashWithSalt i j) (min h h') (P p q)
{-# INLINE pair #-}
