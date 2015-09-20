{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Propagator.Cell where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Primitive.MutVar
import Data.Propagator.Class

-- TODO: use a real mutable hash table
data Cell s a = Cell 
  (a -> a -> Change a)
  {-# UNPACK #-} !(MutVar s (Maybe a, a -> ST s ())) 

instance Eq (Cell s a) where
  Cell _ ra == Cell _ rb = ra == rb

cell :: Propagated a => ST s (Cell s a)
cell = cellWith merge

cellWith :: (a -> a -> Change a) -> ST s (Cell s a)
cellWith mrg = Cell mrg <$> newMutVar (Nothing, \_ -> return ())

-- this will never, ever change
known :: Propagated a => a -> ST s (Cell s a)
known a = Cell merge <$> newMutVar (Just a, \_ -> return ())

write :: Cell s a -> a -> ST s ()
write (Cell m r) a' = join $ atomicModifyMutVar' r $ \case
  (Nothing, ns) -> ((Just a', ns), ns a')
  old@(Just a, ns) -> case m a a' of
    Contradiction e  -> (old, fail e)
    Change False _   -> (old, return ())
    Change True a''  -> ((Just a'', ns), ns a'')

unify :: Cell s a -> Cell s a -> ST s ()
unify x y = do
  watch x (write y)
  watch y (write x)

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
