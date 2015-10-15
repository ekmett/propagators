{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
module Model.IVar where

import Control.Monad (join)
import Control.Monad.IO.Class
import Data.IORef
import Data.Foldable
import Model.Internal.Counted
import Model.Internal.Fiber
import Model.Par

data IVar a = IVar (IORef (Either a (Counted (a -> Fiber ()))))

newIVar :: Par (IVar a)
newIVar = io $ IVar <$> newIORef (Right [])

readIVar :: IVar a -> Par a
readIVar (IVar r) = Par $ \k -> join $ liftIO $ atomicModifyIORef' r $ \case
  l@(Left a) -> (l, k a)
  Right ks   -> (Right (k:+ks), addKarma (-1) >> schedule)

writeIVar :: Eq a => IVar a -> a -> Par ()
writeIVar (IVar r) a = Par $ \k -> join $ liftIO $ atomicModifyIORef' r $ \case
  l@(Left b)
    | a == b    -> (l, k ())
    | otherwise -> (l, fail "writeIVar: mismatch")
  Right ks      -> (Left a, do for_ ks (\k' -> spawn $ k' a); addKarma (length ks); k () )

unsafeWriteIVar :: IVar a -> a -> Par ()
unsafeWriteIVar (IVar r) a = Par $ \k  -> join $ liftIO $ atomicModifyIORef' r $ \case
  l@Left{} -> (l, k ())
  Right ks -> (Left a, do for_ ks (\k' -> spawn $ k' a); addKarma (length ks); k () )

{- -- this is only sound for idempotent Par, but we could use it for a propagator monad
instance MonadZip Par where
  mzipWith f m n = do
    r <- newIVar
    fork $ do
      f <- m
      unsafeWriteIVar f
    a <- n
    f <- readIVar r
    return (f a)

  munzip p = (fmap fst p, fmap snd p)
-}

