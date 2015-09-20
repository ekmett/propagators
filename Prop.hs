{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Primitive
import Data.Primitive.MutVar
import Data.Proxy
import Data.Ratio
import Data.Reify
import Unsafe.Coerce

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
instance Merging Double
instance Merging Rational
instance Merging Float

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

require :: Cell s a -> ST s (Maybe a)
require (Cell _ c) = fst <$> readMutVar c

watch :: Cell s a -> (a -> ST s ()) -> ST s ()
watch (Cell _ r) k = join $ atomicModifyMutVar' r $ \case
  (Nothing, ok)     -> ((Nothing, \a -> k a >> ok a), return ())
  (ma@(Just a), ok) -> ((ma, \a' -> k a' >> ok a'), k a)

with :: Cell s a -> (a -> ST s ()) -> ST s ()
with (Cell _ r) k = do
  p <- readMutVar r
  traverse_ k (fst p)

watch2 :: Cell s a -> Cell s b -> (a -> b -> ST s ()) -> ST s ()
watch2 x y f = do
  watch x $ \a -> with y $ \b -> f a b
  watch y $ \b -> with x $ \a -> f a b

lift1 :: (a -> b) -> Cell s a -> Cell s b -> ST s ()
lift1 f x y = watch x $ \a -> write y (f a)

lift2 :: (a -> b -> c) -> Cell s a -> Cell s b -> Cell s c -> ST s ()
lift2 f x y z = watch2 x y $ \a b -> write z (f a b)

-- propagators via observable sharing 

data Tape s f a where
  Nullary :: ST s (Cell s a) -> Tape s f a
  Unary   :: Merging b => (Cell s a -> Cell s b -> ST s ()) -> f a -> Tape s f b
  Binary  :: Merging c => (Cell s a -> Cell s b -> Cell s c -> ST s ()) -> f a -> f b -> Tape s f c

newtype Prop s a = Prop { unProp :: Tape s (Prop s) a }

mapTape :: (forall a. f a -> g a) -> Tape s f a -> Tape s g a
mapTape _ (Nullary u) = Nullary u
mapTape f (Unary k a) = Unary k (f a)
mapTape f (Binary k a b) = Binary k (f a) (f b)

binary :: Merging c =>(Cell s a -> Cell s b -> Cell s c -> ST s ()) -> Prop s a -> Prop s b -> Prop s c
binary f a b = Prop (Binary f a b)

unary :: Merging b => (Cell s a -> Cell s b -> ST s ()) -> Prop s a -> Prop s b
unary f a = Prop (Unary f a)

nullary :: ST s (Cell s a) -> Prop s a
nullary m = Prop (Nullary m)

instance (Merging a, Eq a, Num a) => Num (Prop s a) where
  (+) = binary $ \x y z -> do
    lift2 (+) x y z
    lift2 (-) z x y
    lift2 (-) z y x
  (-) = binary $ \z x y -> do
    lift2 (+) x y z
    lift2 (-) z x y
    lift2 (-) z y x
  -- TODO: this needs knowledge of if we're fractional or not to do a better job
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

instance (Merging a, Eq a, Fractional a) => Fractional (Prop s a) where
  (/) = binary $ \x y z -> do
    watch2 x y $ \ a b -> when (b /= 0) $ write z (a / b)
    watch2 x z $ \ a c -> when (c /= 0) $ write y (a / c)
    watch2 y z $ \ b c -> write x (b * c)
  recip = unary $ \x y -> do
    watch x $ \ a -> when (a /= 0) $ write y (recip a)
    watch y $ \ b -> when (b /= 0) $ write x (recip b)
  fromRational r = nullary (known $ fromRational r)
  
data UnsafeDerefProp s u where
  UnsafeDerefNullary :: ST s (Cell s a) -> UnsafeDerefProp s u
  UnsafeDerefUnary   :: Merging b => Proxy b -> (Cell s a -> Cell s b -> ST s ()) -> u -> UnsafeDerefProp s u
  UnsafeDerefBinary  :: Merging c => Proxy c -> (Cell s a -> Cell s b -> Cell s c -> ST s ()) -> u -> u -> UnsafeDerefProp s u

instance Functor (UnsafeDerefProp s) where
  fmap _ (UnsafeDerefNullary u)          = UnsafeDerefNullary u
  fmap f (UnsafeDerefUnary Proxy k a)    = UnsafeDerefUnary Proxy k (f a)
  fmap f (UnsafeDerefBinary Proxy k a b) = UnsafeDerefBinary Proxy k (f a) (f b)

instance Foldable (UnsafeDerefProp s) where
  foldMap _ (UnsafeDerefNullary _)      = mempty
  foldMap f (UnsafeDerefUnary _ _ a)    = f a
  foldMap f (UnsafeDerefBinary _ _ a b) = f a `mappend` f b

instance Traversable (UnsafeDerefProp s) where
  traverse _ (UnsafeDerefNullary u)          = pure $ UnsafeDerefNullary u
  traverse f (UnsafeDerefUnary Proxy k a)    = UnsafeDerefUnary Proxy k <$> f a
  traverse f (UnsafeDerefBinary Proxy k a b) = UnsafeDerefBinary Proxy k <$> f a <*> f b

instance MuRef (Prop s a) where
  type DeRef (Prop s a)            = UnsafeDerefProp s
  mapDeRef f (Prop (Nullary n))    = pure $ UnsafeDerefNullary n
  mapDeRef f (Prop (Unary k a))    = UnsafeDerefUnary Proxy k <$> f a
  mapDeRef f (Prop (Binary k a b)) = UnsafeDerefBinary Proxy k <$> f a <*> f b

data ACell s where
  ACell :: Cell s a -> ACell s 

buildACell :: forall s. UnsafeDerefProp s Int -> ST s (ACell s)
buildACell (UnsafeDerefNullary u) = do
  x <- u 
  return (ACell x)
buildACell (UnsafeDerefUnary (Proxy :: Proxy b) _ _) = do
  (x :: Cell s b) <- cell 
  return (ACell x)
buildACell (UnsafeDerefBinary (Proxy :: Proxy c) _ _ _) = do
  (x :: Cell s c) <- cell 
  return (ACell x)

linkACell :: HashMap Int (ACell s) -> (Int, UnsafeDerefProp s Int) -> ST s ()
linkACell m (z,t) = case t of
  UnsafeDerefNullary{}                       -> return ()
  UnsafeDerefUnary (Proxy :: Proxy b) f x    -> case m HM.! x of
    ACell a -> case m HM.! z of
      ACell b -> f (unsafeCoerce a) (unsafeCoerce b)
  UnsafeDerefBinary (Proxy :: Proxy c) f x y -> case m HM.! x of
    ACell a -> case m HM.! y of
      ACell b -> case m HM.! z of
        ACell c -> f (unsafeCoerce a) (unsafeCoerce b) (unsafeCoerce c)

_2 :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
_2 f (a,b) = (,) a <$> f b

lower :: Prop s a -> ST s (Cell s a)
lower m = do
  Graph kvs root <- unsafePrimToPrim (reifyGraph m)
  kvs' <- traverse (_2 buildACell) kvs
  let m = HM.fromList kvs'
  traverse_ (linkACell m) kvs
  case m HM.! root of
    ACell a -> return (unsafeCoerce a)

arg :: Cell s a -> Prop s a
arg a = nullary (return a)

lower1 :: (Prop s a -> Prop s b) -> Cell s a -> ST s (Cell s b)
lower1 f a = lower (f (arg a))

lower2 :: (Prop s a -> Prop s b -> Prop s c) -> Cell s a -> Cell s b -> ST s (Cell s c)
lower2 f a b = lower (f (arg a) (arg b))

forward :: (Merging a, Merging b) => (forall s. Prop s a -> Prop s b) -> a -> Maybe b
forward f a = runST $ do
  x <- cell
  y <- lower1 f x
  write x a
  require y

backward :: (Merging a, Merging b) => (forall s. Prop s a -> Prop s b) -> b -> Maybe a
backward f b = runST $ do
  x <- cell
  y <- lower1 f x
  write y b
  require x

toFahrenheit :: (Eq a, Fractional a, Merging a) => Prop s a -> Prop s a
toFahrenheit c = c / fromRational (5%9) + 32
