module Model.Subscription where

import Model.Par

newtype Subscription = Subscription { cancel :: Par () }

instance Monoid Subscription where
  mempty = Subscription (return ())
  mappend (Subscription m) (Subscription n) = Subscription (m >> n)
