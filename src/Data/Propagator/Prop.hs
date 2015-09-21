{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Unsafe #-}

module Data.Propagator.Prop
  ( Prop(..)
  , lower, arg
  , lower1, lower2
  , forwards, backwards
  ) where

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

-- | This type allows us to write seemingly normal functional code and glue it together out of smaller
-- propagator templates. Evaluation of these expressions uses <www.ittc.ku.edu/~andygill/papers/reifyGraph.pdf Observable Sharing>.
--
-- * 'Nullary' lifts a computation that will produce a 'Cell' into a 'Prop'.
--
-- * 'Unary' lifts a relationship between 2 cells into 'Prop'.
--
-- * 'Binary' lifts a relationship between 3 cells into 'Prop'.
data Prop s a where
  Nullary :: ST s (Cell s a) -> Prop s a
  Unary   :: Propagated b => (Cell s a -> Cell s b -> ST s ()) -> Prop s a -> Prop s b
  Binary  :: Propagated c => (Cell s a -> Cell s b -> Cell s c -> ST s ()) -> Prop s a -> Prop s b -> Prop s c

instance (PropagatedNum a, Eq a, Num a) => Num (Prop s a) where
  (+) = Binary cplus
  (-) = Binary $ \z x y -> cplus x y z
  (*) = Binary ctimes
  negate = Unary $ \x y -> do
    lift1 negate x y
    lift1 negate y x
  signum = Unary $ \x y -> do
    lift1 signum x y
    watch y $ \b -> when (b == 0) $ write x 0
  abs = Unary cabs
  fromInteger i = Nullary (known $ fromInteger i)

instance (PropagatedNum a, Eq a, Fractional a) => Fractional (Prop s a) where
  (/) = Binary $ \x y z -> ctimes z y x
  recip = Unary $ \ x y -> do
     z <- known 1
     ctimes x y z
  fromRational r = Nullary (known $ fromRational r)

-- | most of these only spit out the primary branch when run backwards
instance (PropagatedFloating a, Eq a, Floating a) => Floating (Prop s a) where
  pi = Nullary (known pi)

  exp = Unary cexp
  log = Unary (flip cexp)

  sqrt = Unary csqrt

  x ** y = exp (x * log y)

  logBase a b = log a / log b

  sin = Unary csin
  cos = Unary ccos
  tan = Unary ctan

  asin = Unary (flip csin)
  acos = Unary (flip ccos)
  atan = Unary (flip ctan)

  sinh = Unary csinh
  cosh = Unary ccosh
  tanh = Unary ctanh

  asinh = Unary (flip csinh)
  acosh = Unary (flip ccosh)
  atanh = Unary (flip ctanh)

data DerefProp s u where
  DerefNullary :: ST s (Cell s a) -> DerefProp s u
  DerefUnary   :: Propagated b => Proxy b -> (Cell s a -> Cell s b -> ST s ()) -> u -> DerefProp s u
  DerefBinary  :: Propagated c => Proxy c -> (Cell s a -> Cell s b -> Cell s c -> ST s ()) -> u -> u -> DerefProp s u

instance Functor (DerefProp s) where
  fmap _ (DerefNullary u)          = DerefNullary u
  fmap f (DerefUnary Proxy k a)    = DerefUnary Proxy k (f a)
  fmap f (DerefBinary Proxy k a b) = DerefBinary Proxy k (f a) (f b)

instance Foldable (DerefProp s) where
  foldMap _ (DerefNullary _)      = mempty
  foldMap f (DerefUnary _ _ a)    = f a
  foldMap f (DerefBinary _ _ a b) = f a `mappend` f b

instance Traversable (DerefProp s) where
  traverse _ (DerefNullary u)          = pure $ DerefNullary u
  traverse f (DerefUnary Proxy k a)    = DerefUnary Proxy k <$> f a
  traverse f (DerefBinary Proxy k a b) = DerefBinary Proxy k <$> f a <*> f b

instance MuRef (Prop s a) where
  type DeRef (Prop s a)     = DerefProp s
  mapDeRef _ (Nullary n)    = pure $ DerefNullary n
  mapDeRef f (Unary k a)    = DerefUnary Proxy k <$> f a
  mapDeRef f (Binary k a b) = DerefBinary Proxy k <$> f a <*> f b

data ACell s where
  ACell :: Cell s a -> ACell s

buildACell :: forall s. DerefProp s Int -> ST s (ACell s)
buildACell (DerefNullary u) = do
  x <- u
  return (ACell x)
buildACell (DerefUnary (Proxy :: Proxy b) _ _) = do
  (x :: Cell s b) <- cell
  return (ACell x)
buildACell (DerefBinary (Proxy :: Proxy c) _ _ _) = do
  (x :: Cell s c) <- cell
  return (ACell x)

linkACell :: HashMap Int (ACell s) -> (Int, DerefProp s Int) -> ST s ()
linkACell m (z,t) = case t of
  DerefNullary{}                       -> return ()
  DerefUnary (Proxy :: Proxy b) f x    -> case m HM.! x of
    ACell a -> case m HM.! z of
      ACell b -> f (unsafeCoerce a) (unsafeCoerce b)
  DerefBinary (Proxy :: Proxy c) f x y -> case m HM.! x of
    ACell a -> case m HM.! y of
      ACell b -> case m HM.! z of
        ACell c -> f (unsafeCoerce a) (unsafeCoerce b) (unsafeCoerce c)

_2 :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
_2 f (a,b) = (,) a <$> f b

-- | Lower a 'Prop' to its output 'Cell' by observable sharing.
lower :: Prop s a -> ST s (Cell s a)
lower m = do
  Graph kvs root <- unsafePrimToPrim (reifyGraph m)
  kvs' <- traverse (_2 buildACell) kvs
  let hm = HM.fromList kvs'
  traverse_ (linkACell hm) kvs
  case hm HM.! root of
    ACell a -> return (unsafeCoerce a)

-- | Lift a 'Cell' into a 'Prop'
arg :: Cell s a -> Prop s a
arg a = Nullary (return a)

-- | Lower a unary 'Prop' computation to a relationship between two cells.
lower1 :: (Prop s a -> Prop s b) -> Cell s a -> ST s (Cell s b)
lower1 f a = lower (f (arg a))

-- | Lower a binary 'Prop' computation to a relationship between three cells.
lower2 :: (Prop s a -> Prop s b -> Prop s c) -> Cell s a -> Cell s b -> ST s (Cell s c)
lower2 f a b = lower (f (arg a) (arg b))

-- | Run a 'Prop' formula forwards.
--
-- >>> forwards (\c -> c * 9/5 + 32) 100
-- Just 212.0
forwards :: (Propagated a, Propagated b) => (forall s. Prop s a -> Prop s b) -> a -> Maybe b
forwards f a = runST $ do
  x <- cell
  y <- lower1 f x
  write x a
  content y

-- | Run a 'Prop' formula backwards.
--
-- >>> backwards (\c -> c * 9/5 + 32) 212
-- Just 100.0
backwards :: (Propagated a, Propagated b) => (forall s. Prop s a -> Prop s b) -> b -> Maybe a
backwards f b = runST $ do
  x <- cell
  y <- lower1 f x
  write y b
  content x
