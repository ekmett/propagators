{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
-- | Inspired by the names in nominal adapton.
module Data.Propagator.Name
  ( Name
  , fresh
  , stable
  , fork
  , pair
  , child
  , children
  , height
  ) where

import Control.Concurrent.Unique
import Control.Monad.Primitive
import Data.Bits
import Data.Hashable
import Data.String
import System.Mem.StableName
import GHC.Exts(Any, unsafeCoerce#)

data Name
  = U {-# UNPACK #-} !Unique
  | SN {-# UNPACK #-} !(StableName Any)
  | S {-# UNPACK #-} !Int String
  | P {-# UNPACK #-} !Int {-# UNPACK #-} !Int !Name !Name
  | C {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int !Name

instance Eq Name where
  U i       == U j       = i == j
  SN i      == SN j      = i == j
  S i s     == S j t     = i == j && s == t
  P i _ p q == P j _ r s = i == j && p == r && q == s
  C i _ m p == C j _ n q = i == j && m == n && p == q
  _ == _ = False

instance Show Name where
  showsPrec _ (U i)       = showString "#" . showsPrec 11 (hash i)
  showsPrec _ (SN i)      = showString "%" . showsPrec 11 (hashStableName i)
  showsPrec d (S _ s)     = showsPrec d s
  showsPrec d (P i h l r) = showParen (d > 10) $ showString "P " . showsPrec 11 i . showChar ' ' . showsPrec 11 h . showChar ' ' . showsPrec 11 l . showChar ' ' . showsPrec 11 r
  showsPrec d (C i h n p) = showParen (d > 10) $ showString "C " . showsPrec 11 i . showChar ' ' . showsPrec 11 h . showChar ' ' . showsPrec 11 n . showChar ' ' . showsPrec 11 p

-- | Has a negative binomial distribution. Same for any forked children.
height :: Name -> Int
height (U i)   = ffs (hash i)
height (SN i)  = ffs (hashStableName i)
height (S i _) = ffs i
height (P _ h _ _) = h
height (C _ h _ _) = h
{-# INLINE height #-}

instance Hashable Name where
  hashWithSalt d (U i) = hashWithSalt d i
  hashWithSalt d (SN i) = hashWithSalt d i
  hashWithSalt d (S i _) = hashWithSalt d i
  hashWithSalt d (P i _ _ _) = hashWithSalt d i
  hashWithSalt d (C i _ _ _) = hashWithSalt d i
  {-# INLINE hashWithSalt #-}
  hash (U i) = hash i
  hash (SN i) = hashStableName i
  hash (S i _) = i
  hash (P i _ _ _) = i
  hash (C i _ _ _) = i
  {-# INLINE hash #-}

-- | \"find first set\"
ffs :: Int -> Int
ffs 0 = 0
ffs x = countTrailingZeros x + 1
{-# INLINE ffs #-}

instance IsString Name where
  fromString s = S (hash s) s
  {-# INLINE fromString #-}

-- | Generate a fresh name.
fresh :: PrimMonad m => m Name
fresh = unsafePrimToPrim $ U <$> newUnique
{-# INLINE fresh #-}

-- | Obtain a stable name.
stable :: PrimMonad m => a -> m Name
stable a = unsafePrimToPrim $ SN <$> (unsafeCoerce# makeStableName a :: IO (StableName Any))
{-# INLINE stable #-}

-- | Obtain the name of two children deterministically.
fork :: Name -> (Name, Name)
fork n = (child n 1, child n 2)
{-# INLINE fork #-}

-- | Obtain the name of the kth child.
child :: Name -> Int -> Name
child n d = C (hashWithSalt d n) (height n) d n
{-# INLINE child #-}

-- | Obtain a list of the names of all children
children :: Name -> [Name]
children n = map (\d -> C (hashWithSalt d i) h d n) [1..] where
  i = hash n
  h = height n
{-# INLINE children #-}

-- | build a name based on two existing names
pair :: Name -> Name -> Name
pair m n = P (hash m `hashWithSalt` n) (height m `min` height n) m n
{-# INLINE pair #-}
