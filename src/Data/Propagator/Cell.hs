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

-- | A 'Cell' contains "information about a value" rather than a value per se.
data Cell s a = Cell
  (a -> a -> Change a)
  {-# UNPACK #-} !(MutVar s (Maybe a, a -> ST s ()))

instance Eq (Cell s a) where
  Cell _ ra == Cell _ rb = ra == rb

-- | Construct a new 'Cell' with no information.
cell :: Propagated a => ST s (Cell s a)
cell = cellWith merge

-- | Construct a new 'Cell' with a custom merge strategy.
cellWith :: (a -> a -> Change a) -> ST s (Cell s a)
cellWith mrg = Cell mrg <$> newMutVar (Nothing, \_ -> return ())

-- | Construct a 'Cell' with some information
known :: Propagated a => a -> ST s (Cell s a)
known a = Cell merge <$> newMutVar (Just a, \_ -> return ())
-- known a = do x <- cell; write x a

-- | Writing to a 'Cell' tells it information about its value.
write :: Cell s a -> a -> ST s ()
write (Cell m r) a' = join $ atomicModifyMutVar' r $ \case
  (Nothing, ns) -> ((Just a', ns), ns a')
  old@(Just a, ns) -> case m a a' of
    Contradiction e  -> (old, fail e)
    Change False _   -> (old, return ())
    Change True a''  -> ((Just a'', ns), ns a'')

-- | Unifying two cells makes them exchange information as if they were one 'Cell'.
unify :: Cell s a -> Cell s a -> ST s ()
unify x y = do
  watch x (write y)
  watch y (write x)

-- | Extract the 'content' of a 'Cell'.
content :: Cell s a -> ST s (Maybe a)
content (Cell _ c) = fst <$> readMutVar c

-- | Watching a 'Cell' sets up a callback that will be notified if the cell changes.
watch :: Cell s a -> (a -> ST s ()) -> ST s ()
watch (Cell _ r) k = join $ atomicModifyMutVar' r $ \case
  (Nothing, ok)     -> ((Nothing, \a -> k a >> ok a), return ())
  (ma@(Just a), ok) -> ((ma, \a' -> k a' >> ok a'), k a)

-- | 'with' will read the current value of a 'Cell' and do something with that result
-- if it is known. If the 'Cell' is currently empty, this will do nothing. Unlike
-- watch it does not install a handler.
with :: Cell s a -> (a -> ST s ()) -> ST s ()
with (Cell _ r) k = do
  p <- readMutVar r
  traverse_ k (fst p)

-- | 'watch2' will watch two cells. When they both have some information the supplied
-- callback will fire at least once. It will continue to fire each time they get
-- more information from then out.
watch2 :: Cell s a -> Cell s b -> (a -> b -> ST s ()) -> ST s ()
watch2 x y f = do
  watch x $ \a -> with y $ \b -> f a b
  watch y $ \b -> with x $ \a -> f a b

-- | Lift a unary function into a relationship between two cells.
lift1 :: (a -> b) -> Cell s a -> Cell s b -> ST s ()
lift1 f x y = watch x $ \a -> write y (f a)

-- | Lift a binary function into a relationship between two cells.
lift2 :: (a -> b -> c) -> Cell s a -> Cell s b -> Cell s c -> ST s ()
lift2 f x y z = watch2 x y $ \a b -> write z (f a b)
