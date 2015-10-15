module Model.Observer 
  ( Observer(..)
  , filtered
  ) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
import Model.Par

newtype Observer a = Observer { (!) :: a -> Par () }

instance Contravariant Observer where
  contramap f (Observer g) = Observer (g . f)

instance Divisible Observer where
  conquer = Observer $ \ _ -> return ()
  divide f m n = Observer $ \ a -> case f a of
    (b, c) -> do m!b; n!c

instance Decidable Observer where
  lose f = Observer $ absurd . f
  choose f m n = Observer $ \ a -> case f a of
    Left b  -> m!b
    Right c -> n!c

-- | Primarily used for filtering inputs to observers
filtered :: Decidable f => (a -> Bool) -> f a -> f a
filtered p = choose (\a -> if p a then Right a else Left ()) conquer
