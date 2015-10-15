module Model.Network where

newtype Par s a = Par 

-- we need to be able to talk about the incoming edges and outgoing edges of a cell.

newtype Subscription s = Subscription { cancel :: Par s () }

newtype Observable s a = Observable { subscribe :: Observer a -> Par s (Subscription s) }

instance Functor Observable where
  fmap f (Observable m) = Observable (m . contramap f)

newtype Observer s a = Observer { (!) :: a -> Par s () }

instance Contravariant Observer where
  contramap f (Observer g) = Observer (g . f)

instance Divisible Observer where
  conquer = Observer $ \ _ -> return ()
  divide f m n = Observer $ \ a -> case f a of
    (b, c) -> do m!b; n!c

instnace Decidable Observer where
  lose f = Observer $ absurd . f
  choose f m n = Observer $ \ a -> case f a of
    Left b  -> m!b
    Right c -> n!c

inewtype Par 

