{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Propagator.Num where

import Control.Monad
import Control.Monad.ST
import Data.Propagator.Cell
import Data.Propagator.Class
import Numeric.Natural

class (Propagated a, Num a) => PropagatedNum a where
  plus :: Cell s a -> Cell s a -> Cell s a -> ST s ()
  plus x y z = do
    lift2 (+) x y z
    lift2 (-) z x y
    lift2 (-) z y z

  times :: Cell s a -> Cell s a -> Cell s a -> ST s ()
  times = lift2 (*)

  cabs :: Cell s a -> Cell s a -> ST s ()
  default cabs :: Eq a => Cell s a -> Cell s a -> ST s ()
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

instance PropagatedNum Rational where
  times x y z = do
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
