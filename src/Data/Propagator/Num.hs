{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Propagator.Num where

import Control.Monad
import Control.Monad.ST
import Data.Propagator.Cell
import Data.Propagator.Class
import Numeric.Natural

class Propagated a => PropagatedNum a where
  plus :: Cell s a -> Cell s a -> Cell s a -> ST s ()
  default plus :: Num a => Cell s a -> Cell s a -> Cell s a -> ST s ()
  plus x y z = do
    lift2 (+) x y z
    lift2 (-) z x y
    lift2 (-) z y x

  times :: Cell s a -> Cell s a -> Cell s a -> ST s ()
  default times :: Num a => Cell s a -> Cell s a -> Cell s a -> ST s ()
  times = lift2 (*)

  cabs :: Cell s a -> Cell s a -> ST s ()
  default cabs :: (Num a, Eq a) => Cell s a -> Cell s a -> ST s ()
  cabs x y = do
    lift1 abs x y
    watch y $ \b -> when (b == 0) $ write x 0

instance PropagatedNum Integer where
  times x y z = do
    lift2 (*) x y z
    watch z $ \c -> if c == 0
      then watch x $ \ a -> when (a /= 0) $ write y 0
      else watch y $ \ b -> when (b /= 0) $ write x 0

instance PropagatedNum Natural where
  times x y z = do
    lift2 (*) x y z
    watch z $ \c -> if c == 0
      then watch x $ \ a -> when (a /= 0) $ write y 0
      else watch y $ \ b -> when (b /= 0) $ write x 0
  cabs = unify

instance PropagatedNum Int

instance PropagatedNum Word where
  cabs = unify

timesFractional :: (Eq a, Fractional a) => Cell s a -> Cell s a -> Cell s a -> ST s ()
timesFractional x y z = do
  watch x $ \a ->
    if a == 0
    then write z 0
    else do
      with y $ \b -> write z (a*b)
      with z $ \c -> write y (c/a) -- a /= 0 determined above
  watch y $ \b ->
    if b == 0
    then write z 0
    else do
      with x $ \a -> write z (a*b)
      with z $ \c -> write x (c/b) -- b /= 0 determined above
  watch z $ \c -> do
    with x $ \a -> when (a /= 0) $ write y (c/a)
    with y $ \b -> when (b /= 0) $ write x (c/b)

instance PropagatedNum Rational where
  times = timesFractional

instance PropagatedNum Double where
  times = timesFractional

instance PropagatedNum Float where
  times = timesFractional

class PropagatedNum a => PropagatedFloating a where
  csin :: Cell s a -> Cell s a -> ST s ()
  default csin :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  csin x y = do
    lift1 sin x y
    watch y $ \b -> do
       unless (abs b <= 1) $ fail "output of sin not between -1 and 1"
       write x (asin b)

  ccos :: Cell s a -> Cell s a -> ST s ()
  default ccos :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  ccos x y = do
    lift1 cos x y
    watch y $ \b -> do
       unless (abs b <= 1) $ fail "output of cos not between -1 and 1"
       write x (acos b)

  ctan :: Cell s a -> Cell s a -> ST s ()
  default ctan :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  ctan x y = do
    lift1 tan x y
    watch y $ \b -> do
      unless (abs b <= pi/2) $ fail "output of tan not between -pi/2 and pi/2"
      write x (atan b)

  casin :: Cell s a -> Cell s a -> ST s ()
  default casin :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  casin y x = do
    lift1 sin x y
    watch y $ \b -> do
       unless (abs b <= 1) $ fail "input to asin not between -1 and 1"
       write x (asin b)

  cacos :: Cell s a -> Cell s a -> ST s ()
  default cacos :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  cacos y x = do
    lift1 cos x y
    watch y $ \b -> do
       unless (abs b <= 1) $ fail "input to acos not between -1 and 1"
       write x (acos b)

  catan :: Cell s a -> Cell s a -> ST s ()
  default catan :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  catan y x = do
    lift1 tan x y
    watch y $ \b -> do
      unless (abs b <= pi/2) $ fail "output of tan not between -pi/2 and pi/2"
      write x (atan b)

  csinh :: Cell s a -> Cell s a -> ST s ()
  default csinh :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  csinh x y = do
    lift1 sinh x y
    lift1 asinh y x

  ccosh :: Cell s a -> Cell s a -> ST s ()
  default ccosh :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  ccosh x y = do
    lift1 cosh x y
    watch y $ \b -> do
      unless (b >= 1) $ fail "output of cosh not >= 1"
      lift1 acosh y x

  ctanh :: Cell s a -> Cell s a -> ST s ()
  default ctanh :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  ctanh x y = do
    lift1 tanh x y
    watch y $ \b -> do
      unless (abs b <= 1) $ fail "output of tanh not between -1 and 1"
      write x (tanh b)

  casinh :: Cell s a -> Cell s a -> ST s ()
  default casinh :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  casinh x y = do
    lift1 asinh x y
    lift1 sinh y x

  cacosh :: Cell s a -> Cell s a -> ST s ()
  default cacosh :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  cacosh y x = do
    lift1 cosh x y
    watch y $ \b -> do
      unless (b >= 1) $ fail "input of acosh not >= 1"
      lift1 acosh y x

  catanh :: Cell s a -> Cell s a -> ST s ()
  default catanh :: (Floating a, Ord a) => Cell s a -> Cell s a -> ST s ()
  catanh y x = do
    lift1 tanh x y
    watch y $ \b -> do
      unless (abs b <= 1) $ fail "input of atanh not between -1 and 1"
      write x (tanh b)

instance PropagatedFloating Float
instance PropagatedFloating Double
