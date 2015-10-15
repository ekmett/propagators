{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model.Par where

import Control.Concurrent
import Control.Exception
import Control.Monad (join, when)
import Control.Monad.IO.Class
import Data.IORef
import Data.Foldable
import Data.Primitive.Array
import Data.Traversable (for)
import Model.Internal.Counted
import Model.Internal.Deque as Deque
import Model.Internal.Fiber
import System.Random.MWC as MWC

newtype Par a = Par { unPar :: (a -> Fiber ()) -> Fiber () }
  deriving Functor

instance Applicative Par where
  pure x  = Par (\k -> k x)
  Par f <*> Par v = Par $ \ c -> f $ \ g -> v (c . g)

instance Monad Par where
  return x = Par (\k -> k x)
  Par m >>= k  = Par $ \ c -> m (\ x -> unPar (k x) c)

-- | I need a simple form of IVar in order to implement MonadZip

data BlockedIndefinitelyOnIVar = BlockedIndefinitelyOnIVar deriving (Show,Exception)

data IVar a = IVar (IORef (Either a (Counted (a -> Fiber ()))))

-- How can we attach a finalizer that looks for stuck fibers?

newIVar :: Par (IVar a)
newIVar = io $ IVar <$> newIORef (Right [])
{-
newIVar = io $ do
  r <- newIORef (Right [])
  m <- newEmptyMVar 
  w <- mkWeakIORef r $ do
    w <- readMVar m 
    deRefWeak w >>= \case
      Nothing -> do putStrLn "finalizer: dead weak reference"; fail "died"
      Just r' -> readIORef r' >>= \case
        Right n | not (null n) -> do putStrLn "blocked"; throwIO BlockedIndefinitelyOnIVar
        _                      -> return ()
  putMVar m w
  return (IVar r)
-}

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

{- -- this is only sound for idempotent Par
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

-- -- | Returns the net karma. If negative there remain blocked computations.
runPar_ :: Par a -> IO ()
runPar_ (Par m) = do
  idlers <- newIORef []
  n <- getNumCapabilities
  d <- if n == 1 then do
    -- putStrLn "1 capability"
    pool <- Deque.empty
    seed <- MWC.create
    karma <- newIORef 0
    workers <- newArray 0 (error "PANIC! runPar_ missing worker 0")
    runFiber (m $ const schedule) Worker { ident=0, .. }
    readIORef karma
  else do
    -- putStrLn $ show n ++ " capabilities"
    tid <- myThreadId
    (k,_locked) <- threadCapability tid
    ws <- for [0..n-1] $ \ident -> do
      pool <- Deque.empty
      seed <- MWC.create
      karma <- newIORef 0
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
       forkOn (k + 1 + ident i) (runFiber schedule i) -- distribute the other workers to other capabilities mod n
    runFiber (m $ const schedule) lws                 -- process the last thing locally for now.
    foldlM (\x i -> do y <- readIORef (karma i); return $! x + y) 0 ws
  -- putStrLn $ "karma: " ++ show d
  when (d < 0) $ throwIO BlockedIndefinitelyOnIVar

runPar :: Par a -> IO a
runPar m = do
  r <- newEmptyMVar
  runPar_ $ do
     a <- m
     io (putMVar r a)
  readMVar r
