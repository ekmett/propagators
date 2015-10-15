{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
module Model.Internal.Task where

import Control.Concurrent.MVar
import Control.Monad (ap, join, when)
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Maybe
import Data.Primitive.Array
import Model.Internal.Counted
import Model.Internal.Deque as Deque
import Model.Internal.Util
import System.Random.MWC

-- internal state of a Worker
data Worker = Worker
  { ident   :: {-# UNPACK #-} !Int
  , pool    :: !(Deque (Task ()))
  , workers :: !(MutableArray RealWorld Worker) -- Other Workers. They will get shuffled as we schedule work stealing
  , idlers  :: !(IORef (Counted (MVar (Task ()))))
  , seed    :: Gen RealWorld
  }

-- TODO: change workers to just contain an IO action that can do stealing. This prevents us from holding the entire other worker alive and makes a safer back-end.

newtype Task a = Task { runTask :: Worker -> IO a }
  deriving Functor

instance Applicative Task where
  pure a = Task $ \ _ -> pure a
  (<*>) = ap -- TODO: can we just parallelize here if we don't allow any true sequencing operations?

instance Monad Task where
  Task m >>= f = Task $ \s -> do
    a <- m s
    runTask (f a) s
  fail e = Task $ \ _ -> fail e

instance MonadIO Task where
  liftIO m = Task $ \_ -> m

instance PrimMonad Task where
  type PrimState Task = RealWorld
  primitive f = Task $ \ _ -> primitive f

-- | Look for something to do locally, otherwise go look for work
schedule :: Task ()
schedule = Task $ \ s@Worker{..} -> pop pool >>= \case
  Just t -> runTask t s
  Nothing
    | n == 0    -> return ()
    | otherwise -> runTask (interview (n-1)) s
    where n = sizeofMutableArray workers

-- | Go door to door randomly looking for work. Requires there to be at least one door to knock on.
interview :: Int -> Task ()
interview i
  | i == 0 = Task $ \s@Worker{workers} -> do
    b <- readArray workers 0
    m <- steal (pool b)
    runTask (fromMaybe idle m) s
  | otherwise = Task $ \ s@Worker{workers, seed} -> do
    j <- uniformR (0,i) seed -- perform an on-line Knuth shuffle step
    a <- readArray workers i
    b <- readArray workers j
    writeArray workers i b
    writeArray workers j a
    m <- steal (pool b)
    runTask (fromMaybe (interview (i-1)) m) s

-- | We have a hot tip from somebody with a job opening!
referral :: Worker -> Task ()
referral b = do
  m <- steal (pool b)
  fromMaybe schedule m

-- | Give up and wait for somebody to wake us up.
idle :: Task ()
idle = Task $ \s@Worker{..} -> do
  m <- newEmptyMVar
  is <- atomicModifyIORef idlers $ \is -> (m :+ is,is)
  if length is == sizeofMutableArray workers
  then for_ is $ \n -> putMVar n $ pure () -- shut it down. we're the last idler.
  else do
    t <- takeMVar m
    runTask t s

-- | Spawn a background task. We first put it into our job queue, and then we wake up an idler if there are any and have them try to steal it.
spawn :: Task () -> Task ()
spawn t = Task $ \ s@Worker{idlers,pool} -> do
  xs <- readIORef idlers
  push t pool
  when (not (Prelude.null xs)) $
    join $ atomicModifyIORef idlers $ \case
       i:+is -> (is, putMVar i (referral s))
       _     -> ([], return ())
