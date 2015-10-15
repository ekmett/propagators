{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
module Model.Par where

import Control.Concurrent.MVar
import Control.Monad (join, when)
import Control.Monad.Primitive
import Data.Foldable
import Data.IORef
import Data.Primitive.Array
import Model.Array
import Model.Counted
import Model.Deque as Deque
import System.Random.MWC

type Task = Worker -> IO ()

-- internal state of a Worker
data Worker = Worker
  { ident   :: {-# UNPACK #-} !Int
  , pool    :: !(Deque Task)
  , workers :: !(MutableArray RealWorld Worker) -- other Workers. They will get shuffled as we schedule work stealing
  , idlers  :: !(IORef (Counted (MVar Task)))
  , seed    :: Gen RealWorld
  } 

-- | this will do work stealing scheduling
schedule :: Task
schedule s@Worker{..} = pop pool >>= \case
  Just t -> t s
  Nothing
    | n == 0    -> return ()
    | otherwise -> interview (n-1) s
    where n = sizeofMutableArray workers

-- | go door to door randomly looking for work
interview :: Int -> Task 
interview i s@Worker{workers,seed}
  | i == 0 = do
    b <- readArray workers 0
    steal (pool b) >>= \case
      Just t -> t s
      Nothing -> idle s
  | otherwise = do
    j <- uniformR (0,i) seed
    a <- readArray workers i 
    b <- readArray workers j
    writeArray workers i b 
    writeArray workers j a
    steal (pool b) >>= \case
      Just t -> t s
      Nothing -> interview (i-1) s

-- | We have a hot tip from somebody with a job opening
referral :: Worker -> Task
referral b s = steal (pool b) >>= \case
  Just t -> t s
  Nothing -> schedule s
  
-- | Give up
idle :: Task
idle s@Worker{..} = do
  m <- newEmptyMVar
  is <- atomicModifyIORef idlers $ \is -> (m :+ is,is)
  if length is == sizeofMutableArray workers
  then for_ is $ \n -> putMVar n $ \_ -> return () -- shut it down. we're the last idler.
  else do
    t <- takeMVar m
    t s 

-- | spawn a background task. We first put it into our job queue, and then we wake up an idler if there are any and have them try to steal it.
spawn :: Task -> Task
spawn t s@Worker{idlers,pool} = do
  xs <- readIORef idlers
  push t pool
  when (not (Prelude.null xs)) $
    join $ atomicModifyIORef idlers $ \case
       i:+is -> (is, putMVar i (referral s))
       _     -> ([], return ())

newtype Par a = Par { unPar :: (a -> Task) -> Task }
  deriving Functor

instance Applicative Par where
  pure x  = Par (\k -> k x)
  Par f <*> Par v = Par $ \ c -> f $ \ g -> v (c . g)

instance Monad Par where
  return x = Par (\k -> k x)
  Par m >>= k  = Par $ \ c -> m (\ x -> unPar (k x) c)

fork :: Par a -> Par ()
fork (Par m) = Par $ \k s -> do
  spawn (k ()) s
  m (const schedule) s
  
-- | embed an IO action. (This is rather unsafe)
io :: IO a -> Par a
io m = Par $ \k s -> do
  a <- m 
  k a s
