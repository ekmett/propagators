-- | Inspired by the names in nominal adapton.
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
import Data.IORef
import Data.Int
import Data.String
import System.IO.Unsafe

supply :: IORef Int64
supply = unsafePerformIO (newIORef 0)
{-# NOINLINE supply #-}

data Path
  = U {-# UNPACK #-} !Int64
  | S String
  | P !Path !Path
  | C {-# UNPACK #-} !Int Path
  deriving (Show, Eq)

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

-- | gensym
fresh :: PrimMonad m => m Name
fresh = unsafePrimToPrim $ do
  u <- atomicModifyIORef' supply $ \x -> let z = x+1 in (z,z)
  return $ name (fromIntegral u) (U u)
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
