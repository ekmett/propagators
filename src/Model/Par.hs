{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
module Model.Par where

import Model.Internal.Task

newtype Par a = Par { unPar :: (a -> Task) -> Task }
  deriving Functor

instance Applicative Par where
  pure x  = Par (\k -> k x)
  Par f <*> Par v = Par $ \ c -> f $ \ g -> v (c . g)

instance Monad Par where
  return x = Par (\k -> k x)
  Par m >>= k  = Par $ \ c -> m (\ x -> unPar (k x) c)

-- MonadZip? =)

fork :: Par a -> Par ()
fork (Par m) = Par $ \k s -> do
  spawn (k ()) s
  m (const schedule) s
  
-- | embed an IO action. (This is rather unsafe)
io :: IO a -> Par a
io m = Par $ \k s -> do
  a <- m 
  k a s
