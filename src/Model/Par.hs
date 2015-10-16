{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model.Par where

import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.Foldable
import Data.Primitive.Array
import Data.Traversable (for)
import Model.Internal.Deque as Deque
import Model.Internal.Fiber
import System.Random.MWC as MWC

data BlockedIndefinitelyOnIVar = BlockedIndefinitelyOnIVar deriving (Show,Exception)

-- newtype Par a = Par { unPar :: (a -> Fiber ()) -> (SomeException -> Fiber ()) -> Fiber () }
newtype Par a = Par { unPar :: (a -> Fiber ()) -> Fiber () }
  deriving Functor

instance Applicative Par where
  pure x  = Par (\k -> k x)
  Par f <*> Par v = Par $ \ c -> f $ \ g -> v (c . g)

instance Monad Par where
  return x = Par $ \k -> k x
  fail s = Par $ \_ -> fail s
  Par m >>= k = Par $ \ c -> m (\ x -> unPar (k x) c)

instance MonadThrow Par where
  throwM e = Par $ \_ -> throwM e

-- TODO: MonadZip for parallel monad comprehensions

fork :: Par a -> Par ()
fork (Par m) = Par $ \k -> do
  spawn (k ())
  m (const schedule)

-- | embed an IO action. (This is rather unsafe)
io :: IO a -> Par a
io m = Par $ \k -> do
  a <- liftIO m
  k a

runPar_ :: Par a -> IO ()
runPar_ (Par m) = do
  idlers <- newIORef []
  n <- getNumCapabilities
  tid <- myThreadId
  (k,_) <- threadCapability tid
  ws <- for [0..n-1] $ \ident -> do
    pool <- Deque.empty
    seed <- MWC.create
    karma <- newIORef 0
    workers <- newArray (n-1) (error "PANIC! runPar_ missing worker")
    fast <- newIORef True
    return Worker {..}
  let iws = init ws -- this would be more efficient with head/tail
      lws = last ws
  forM_ ws $ \i -> forM_ iws $ \j -> writeArray (workers i) (ident j) j
  forM_ iws $ \i -> do
    writeArray (workers i) (ident i) lws
    forkOn (k + 1 + ident i) (runFiber schedule i)
  runFiber (m $ const schedule) lws
  d <- foldlM (\x i -> do y <- readIORef (karma i); return $! x + y) 0 ws
  when (d < 0) $ throwIO BlockedIndefinitelyOnIVar

runPar :: Par a -> IO a
runPar m = do
  r <- newEmptyMVar
  runPar_ $ do
     a <- m
     io (putMVar r a)
  readMVar r
