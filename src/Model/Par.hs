{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
module Model.Par where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef
import Data.Foldable
import Data.Primitive.Array
import Data.Traversable (for)
import System.Random.MWC as MWC
import Model.Internal.Deque as Deque
import Model.Internal.Task

newtype Par a = Par { unPar :: (a -> Task) -> Task }
  deriving Functor

instance Applicative Par where
  pure x  = Par (\k -> k x)
  Par f <*> Par v = Par $ \ c -> f $ \ g -> v (c . g)

instance Monad Par where
  return x = Par (\k -> k x)
  Par m >>= k  = Par $ \ c -> m (\ x -> unPar (k x) c)

-- TODO: MonadZip for parallel monad comprehensions

fork :: Par a -> Par ()
fork (Par m) = Par $ \k s -> do
  spawn (k ()) s
  m (const schedule) s

-- | embed an IO action. (This is rather unsafe)
io :: IO a -> Par a
io m = Par $ \k s -> do
  a <- m
  k a s

runPar_ :: Par a -> IO ()
runPar_ (Par m) = do
  idlers <- newIORef []
  n <- getNumCapabilities
  if n == 1 then do
    putStrLn "1 capability"
    pool <- Deque.empty
    seed <- MWC.create
    workers <- newArray 0 (error "PANIC! runPar_ missing worker 0")
    m (const schedule) Worker { ident=0, .. }
  else do
    putStrLn $ show n ++ " capabilities"
    tid <- myThreadId
    (k,_locked) <- threadCapability tid
    ws <- for [0..n-1] $ \ident -> do
      pool <- Deque.empty
      seed <- MWC.create
      workers <- newArray (n-1) $ error $ "PANIC! runPar_ missing worker in " ++ show ident
      return Worker {..}
    -- 01234
    -- becomes
    -- 0: 4123 iws[0]
    -- 1: 0423 iws[1]
    -- 2: 0143 iws[2]
    -- 3: 0124 iws[3]
    -- 4: 0123 lws
    let iws = init ws
        lws = last ws
    forM_ ws $ \i -> forM_ iws $ \j -> writeArray (workers i) (ident j) j
    forM_ iws $ \i -> do
       writeArray (workers i) (ident i) lws
       forkOn (k + 1 + ident i) (schedule i) -- distribute the other workers to other capabilities mod n
    m (const schedule) lws                   -- process the last thing locally for now.

runPar :: Par a -> IO a
runPar m = do
  r <- newEmptyMVar
  runPar_ $ do
     a <- m
     io (putMVar r a)
  readMVar r
