{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
module Model.Internal.Fiber where

import Control.Concurrent.MVar
import Control.Monad (ap, join, unless)
import Control.Monad.Catch
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
  , pool    :: !(Deque (Fiber ()))
  , workers :: !(MutableArray RealWorld Worker) -- Other Workers. They will get shuffled as we schedule work stealing
  , idlers  :: {-# UNPACK #-} !(IORef (Counted (MVar (Fiber ()))))
  , seed    :: !(Gen RealWorld)
  , karma   :: {-# UNPACK #-} !(IORef Int)
  , fast    :: {-# UNPACK #-} !(IORef Bool)
  }

-- TODO: change workers to just contain an IO action that can do stealing. This prevents us from holding the entire other worker alive and makes a safer back-end.

newtype Fiber a = Fiber { runFiber :: Worker -> IO a }
  deriving Functor

instance Applicative Fiber where
  pure a = Fiber $ \ _ -> pure a
  (<*>) = ap -- TODO: can we just parallelize here if we don't allow any true sequencing operations?

instance Monad Fiber where
  Fiber m >>= f = Fiber $ \s -> do
    a <- m s
    runFiber (f a) s
  fail e = Fiber $ \ _ -> fail e

instance MonadThrow Fiber where
  throwM e = Fiber $ \_ -> throwM e

instance MonadCatch Fiber where
  catch (Fiber m) k = Fiber $ \s -> m s `catch` \e -> runFiber (k e) s

instance MonadMask Fiber where
  mask a = Fiber $ \e -> mask $ \u -> runFiber (a $ q u) e
    where q :: (IO a -> IO a) -> Fiber a -> Fiber a
          q u (Fiber b) = Fiber (u . b)
  uninterruptibleMask a = Fiber $ \e -> uninterruptibleMask $ \u -> runFiber (a $ q u) e
    where q :: (IO a -> IO a) -> Fiber a -> Fiber a
          q u (Fiber b) = Fiber (u . b)

instance MonadIO Fiber where
  liftIO m = Fiber (const m)

instance PrimMonad Fiber where
  type PrimState Fiber = RealWorld
  primitive f = Fiber $ \ _ -> primitive f

-- | Look for something to do locally, otherwise go look for work
schedule :: Fiber ()
schedule = Fiber $ \ s@Worker{..} -> pop pool >>= \case
  Just t -> do
    writeIORef fast True
    runFiber t s
  Nothing
    | n > 0 -> do
      writeIORef fast False
      runFiber (interview (n-1)) s
    | otherwise -> return ()
    where n = sizeofMutableArray workers

-- | Go door to door randomly looking for work. Requires there to be at least one door to knock on.
interview :: Int -> Fiber ()
interview i
  | i > 0 = Fiber $ \ s@Worker{workers, seed} -> do
    j <- uniformR (0,i) seed -- perform an on-line Knuth shuffle step
    a <- readArray workers i
    b <- readArray workers j
    writeArray workers i b
    writeArray workers j a
    m <- steal (pool b)
    runFiber (fromMaybe (interview (i-1)) m) s
  | otherwise = Fiber $ \s@Worker{workers} -> do
    b <- readArray workers 0
    m <- steal (pool b)
    runFiber (fromMaybe idle m) s

-- | We have a hot tip from somebody with a job opening!
referral :: Worker -> Fiber ()
referral b = do
  m <- steal (pool b)
  fromMaybe schedule m

-- | Give up and wait for somebody to wake us up.
idle :: Fiber ()
idle = Fiber $ \s@Worker{..} -> do
  m <- newEmptyMVar
  is <- atomicModifyIORef idlers $ \is -> (m :+ is,is)
  if length is == sizeofMutableArray workers
  then for_ is $ \i -> putMVar i $ pure () -- shut it down. we're the last idler.
  else do
    t <- takeMVar m -- We were given this, we didn't steal it. Really!
    writeIORef fast True
    runFiber t s

-- | Spawn a background task. We first put it into our job queue, and then we wake up an idler if there are any and have them try to steal it.
defer :: Fiber () -> Fiber ()
defer t = Fiber $ \ s@Worker{idlers,pool} -> do
  xs <- readIORef idlers
  push t pool
  unless (Prelude.null xs) $
    join $ atomicModifyIORef idlers $ \case
       i:+is -> (is, putMVar i $ referral s)
       _     -> ([], return ())

-- | If the computation ends and we have globally accumulated negative karma then somebody, somewhere, is blocked.
addKarma :: Int -> Fiber ()
addKarma k = Fiber $ \ Worker{karma} -> modifyIORef' karma (k+)
