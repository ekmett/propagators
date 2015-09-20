{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.Primitive
import Data.Primitive.MutVar
import Data.Reify
-- import System.Mem.StableName.Map

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
  Contradiction s <|> n = n
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

class Merging a where
  merge :: a -> a -> Change a
  default merge :: (Eq a, Show a) => a -> a -> Change a
  merge = mergeDefault

instance Merging Int
instance Merging Integer
instance Merging Word

instance (Merging a, Merging b) => Merging (a, b) where
  merge (a,b) (c,d) = (,) <$> merge a c <*> merge b d

instance (Merging a, Merging b) => Merging (Either a b) where
  merge (Left a)  (Left b)  = Left <$> merge a b
  merge (Right a) (Right b) = Right <$> merge a b
  merge _ _ = fail "Left /= Right"

-- TODO: use a real mutable hash table
data Cell s a = Cell 
  (a -> a -> Change a)
  {-# UNPACK #-} !(MutVar s (Maybe a, a -> ST s ())) 

instance Eq (Cell s a) where
  Cell _ ra == Cell _ rb = ra == rb

cell :: Merging a => ST s (Cell s a)
cell = cellWith merge

cellWith :: (a -> a -> Change a) -> ST s (Cell s a)
cellWith mrg = Cell mrg <$> newMutVar (Nothing, \_ -> return ())

-- this will never, ever change
known :: Merging a => a -> ST s (Cell s a)
known a = Cell merge <$> newMutVar (Just a, \_ -> return ())

write :: Cell s a -> a -> ST s ()
write (Cell m r) a' = join $ atomicModifyMutVar' r $ \case
  (Nothing, ns) -> ((Just a', ns), ns a')
  old@(Just a, ns) -> case m a a' of
    Contradiction e -> (old, fail e)
    Change False a'' -> (old, return ())
    Change True a''  -> ((Just a'', ns), ns a'')

readCell :: Cell s a -> ST s (Maybe a)
readCell (Cell _ c) = fst <$> readMutVar c

watch :: Cell s a -> (a -> ST s ()) -> ST s ()
watch (Cell _ r) k = join $ atomicModifyMutVar' r $ \case
  (Nothing, ok)     -> ((Nothing, \a -> k a >> ok a), return ())
  (ma@(Just a), ok) -> ((ma, \a' -> k a' >> ok a'), k a)

with :: Cell s a -> (a -> ST s ()) -> ST s ()
with (Cell _ r) k = do
  p <- readMutVar r
  traverse_ k (fst p)

lift1 :: (a -> b) -> Cell s a -> Cell s b -> ST s ()
lift1 f x y = watch x $ \a -> write y (f a)

lift2 :: (a -> b -> c) -> Cell s a -> Cell s b -> Cell s c -> ST s ()
lift2 f x y z = do
  watch x $ \a -> with y $ \b -> write z (f a b)
  watch y $ \b -> with x $ \a -> write z (f a b)

-- propagators via observable sharing 


data Tape f a where
  Nullary :: (forall s. ST s (Cell s a)) -> Tape f a
  Unary   :: Merging b => (forall s. Cell s a -> Cell s b -> ST s ()) -> f a -> Tape f b
  Binary  :: Merging c => (forall s. Cell s a -> Cell s b -> Cell s c -> ST s ()) -> f a -> f b -> Tape f c

newtype Prop a = Prop { unProp :: Tape Prop a }

mapTape :: (forall a. f a -> g a) -> Tape f a -> Tape g a
mapTape _ (Nullary u) = Nullary u
mapTape f (Unary k a) = Unary k (f a)
mapTape f (Binary k a b) = Binary k (f a) (f b)

binary :: Merging c =>(forall s. Cell s a -> Cell s b -> Cell s c -> ST s ()) -> Prop a -> Prop b -> Prop c
binary f a b = Prop (Binary f a b)

unary :: Merging b => (forall s. Cell s a -> Cell s b -> ST s ()) -> Prop a -> Prop b
unary f a = Prop (Unary f a)

nullary :: (forall s. ST s (Cell s a)) -> Prop a
nullary m = Prop (Nullary m)

instance (Merging a, Eq a, Num a) => Num (Prop a) where
  (+) = binary $ \x y z -> do
    lift2 (+) x y z
    lift2 (-) z x y
    lift2 (-) z y x
  (-) = binary $ \z x y -> do
    lift2 (+) x y z
    lift2 (-) z x y
    lift2 (-) z y x
  (*) = binary $ \x y z -> do
    lift2 (*) x y z
    watch z $ \c -> if c == 0
      then watch x $ \ a -> when (a /= 0) $ write y 0
      else watch y $ \ b -> when (b /= 0) $ write x 0
  negate = unary $ \x y -> lift1 negate x y >> lift1 negate y x
  signum = unary $ \x y -> do
    lift1 signum x y 
    watch y $ \b -> when (b == 0) $ write x 0
  abs = unary $ \x y -> do
    lift1 abs x y
    watch y $ \b -> when (b == 0) $ write x 0
  fromInteger i = nullary (known $ fromInteger i)

{-
data UnsafeDerefProp u where
  UnsafeDerefNullary :: (forall s. ST s (Cell s a)) -> UnsafeDerefProp u
  UnsafeDerefUnary   :: Merging b => (forall s. Cell s a -> Cell s b -> ST s ()) -> u -> UnsafeDerefProp u
  UnsafeDerefBinary  :: Merging c => (forall s. Cell s a -> Cell s b -> Cell s c -> ST s ()) -> u -> u -> UnsafeDerefProp u

instance MuRef (Prop a) where
  type DeRef (Prop a)       = DerefProp a
  mapDeRef f (Nullary n)    = pure $ DerefNullary n
  mapDeRef f (Unary k a)    = DerefUnary k <$> f a
  mapDeRef f (Binary k a b) = DerefBinary k <$> f a <*> f b

Graph DerefProp -> ST s (Prop c)

lower1 :: (Prop a -> Prop b) -> Cell s a -> ST s (Cell s a)
lower1 f a = do
  g <- reifyGraph $ f (Nullary (return a)) 
-}
