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
{-# LANGUAGE Unsafe #-}

module Data.Propagator.Prop where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Propagator.Class
import Data.Propagator.Cell
import Data.Propagator.Num
import Data.Proxy
import Data.Reify
import Unsafe.Coerce

-- propagators via observable sharing 

data Tape s f a where
  Nullary :: ST s (Cell s a) -> Tape s f a
  Unary   :: Propagated b => (Cell s a -> Cell s b -> ST s ()) -> f a -> Tape s f b
  Binary  :: Propagated c => (Cell s a -> Cell s b -> Cell s c -> ST s ()) -> f a -> f b -> Tape s f c

newtype Prop s a = Prop { unProp :: Tape s (Prop s) a }

mapTape :: (forall x. f x -> g x) -> Tape s f a -> Tape s g a
mapTape _ (Nullary u) = Nullary u
mapTape f (Unary k a) = Unary k (f a)
mapTape f (Binary k a b) = Binary k (f a) (f b)

binary :: Propagated c =>(Cell s a -> Cell s b -> Cell s c -> ST s ()) -> Prop s a -> Prop s b -> Prop s c
binary f a b = Prop (Binary f a b)

unary :: Propagated b => (Cell s a -> Cell s b -> ST s ()) -> Prop s a -> Prop s b
unary f a = Prop (Unary f a)

nullary :: ST s (Cell s a) -> Prop s a
nullary m = Prop (Nullary m)

instance (PropagatedNum a, Eq a, Num a) => Num (Prop s a) where
  (+) = binary plus
  (-) = binary $ \z x y -> plus x y z
  (*) = binary times
  negate = unary $ \x y -> do
    lift1 negate x y
    lift1 negate y x
  signum = unary $ \x y -> do
    lift1 signum x y 
    watch y $ \b -> when (b == 0) $ write x 0
  abs = unary cabs
  fromInteger i = nullary (known $ fromInteger i)

instance (PropagatedNum a, Eq a, Fractional a) => Fractional (Prop s a) where
  (/) = binary $ \x y z -> times z y x
  recip = unary $ \x y -> do
    watch x $ \ a -> when (a /= 0) $ write y (recip a)
    watch y $ \ b -> when (b /= 0) $ write x (recip b)
  fromRational r = nullary (known $ fromRational r)
  
data UnsafeDerefProp s u where
  UnsafeDerefNullary :: ST s (Cell s a) -> UnsafeDerefProp s u
  UnsafeDerefUnary   :: Propagated b => Proxy b -> (Cell s a -> Cell s b -> ST s ()) -> u -> UnsafeDerefProp s u
  UnsafeDerefBinary  :: Propagated c => Proxy c -> (Cell s a -> Cell s b -> Cell s c -> ST s ()) -> u -> u -> UnsafeDerefProp s u

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
  mapDeRef _ (Prop (Nullary n))    = pure $ UnsafeDerefNullary n
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
  let hm = HM.fromList kvs'
  traverse_ (linkACell hm) kvs
  case hm HM.! root of
    ACell a -> return (unsafeCoerce a)

arg :: Cell s a -> Prop s a
arg a = nullary (return a)

lower1 :: (Prop s a -> Prop s b) -> Cell s a -> ST s (Cell s b)
lower1 f a = lower (f (arg a))

lower2 :: (Prop s a -> Prop s b -> Prop s c) -> Cell s a -> Cell s b -> ST s (Cell s c)
lower2 f a b = lower (f (arg a) (arg b))

forward :: (Propagated a, Propagated b) => (forall s. Prop s a -> Prop s b) -> a -> Maybe b
forward f a = runST $ do
  x <- cell
  y <- lower1 f x
  write x a
  require y

backward :: (Propagated a, Propagated b) => (forall s. Prop s a -> Prop s b) -> b -> Maybe a
backward f b = runST $ do
  x <- cell
  y <- lower1 f x
  write y b
  require x

-- toFahrenheit :: (Eq a, Fractional a, Propagated a) => Prop s a -> Prop s a
-- toFahrenheit c = c / fromRational (5%9) + 32
