{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-15 Edward Kmett, Ryan Newton
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Chase-Lev work-stealing deques
--
-- This implementation derives directly from the pseudocode in the
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.170.1097&rep=rep1&type=pdf 2005 SPAA paper>.
--
-----------------------------------------------------------------------------
module Model.Internal.Deque 
  ( Deque
  -- * Initialization
  , empty
  , singleton
  , fromList
  , fromListN
  -- * Size
  , null
  , size
  -- * Local Operations
  , push, pushes
  , pop
  -- * Work-Stealing
  , steal
  ) where

import Control.Exception (evaluate)
import Data.Atomics (storeLoadBarrier, writeBarrier, loadLoadBarrier)
import Data.Atomics.Counter
import Data.Foldable hiding (null)
import Data.Functor.Reverse
import Data.IORef
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as MV
import Data.Vector.Mutable (MVector)
import GHC.Prim (RealWorld)
import Model.Internal.Util
import Prelude hiding (null)

-- $setup
-- >>> :set -XScopedTypeVariables -XNoOverloadedStrings

-- | A Chase-Lev circular work-stealing deque
--
-- A work-stealing deque is perhaps more properly referred to as a concurrent steque,
-- where the dequeue-only 'steal' side can be used concurrently, but the \"local\" side may
-- that provides 'push' and 'pop' can only be used from a single owner thread.
data Deque a = Deque
  { bottom, top :: {-# UNPACK #-} !AtomicCounter -- TODO: would it be worth making one larger MutableByteArray# and then putting these at least a cache line apart to avoid conflict?
  , array  :: {-# UNPACK #-} !(IORef (MVector RealWorld a))
  }

-- | Create a new 'empty' 'Deque'.
--
-- >>> q :: Deque Int <- empty
-- >>> null q
-- True
-- >>> pop q
-- Nothing
-- >>> steal q
-- Nothing
empty :: IO (Deque a)
empty = do
  bottom <- newCounter 0 -- try to allocate them a bit separated
  v <- MV.new 32
  array <- newIORef v
  top <- newCounter 0
  return Deque{..}

-- | Create a new 'Deque' with one element in it.
--
-- >>> q <- singleton "hello"
-- >>> size q
-- (1,1)
-- >>> pop q
-- Just "hello"
-- >>> pop q
-- Nothing
singleton :: forall a. a -> IO (Deque a)
singleton a = do
  bottom <- newCounter 1
  v <- MV.new 32
  MV.unsafeWrite v 0 a
  array <- newIORef v
  top <- newCounter 0
  return Deque{..}

-- | Generate a work-stealing 'Deque' of elements from a list.
--
-- >>> q <- fromList [1,2,3,4 :: Int]
-- >>> pop q
-- Just 1
-- >>> pop q
-- Just 2
-- >>> pop q
-- Just 3
-- >>> pop q
-- Just 4
-- >>> pop q
-- Nothing
--
-- >>> p <- fromList [1,2,3,4 :: Int]
-- >>> steal p
-- Just 4
fromList :: forall a. [a] -> IO (Deque a)
fromList as = do
  v <- V.unsafeThaw (V.reverse (V.fromList as :: Vector a)) -- TODO: pad this out to an initial 32 entries?
  bottom <- newCounter (MV.length v)
  array <- newIORef v
  top <- newCounter 0
  return Deque{..}

-- | Generate a work-stealing 'Deque' of elements from a list of known length.
fromListN :: forall a. Int -> [a] -> IO (Deque a)
fromListN n as = do
  v <- V.unsafeThaw (V.reverse (V.fromListN n as :: Vector a))
  bottom <- newCounter (MV.length v)
  array <- newIORef v
  top <- newCounter 0
  return Deque{..}

-- | @null@ returns 'True' if the 'Deque' is definitely empty.
--
-- >>> q <- singleton (1 :: Int)
-- >>> null q
-- False
-- >>> pop q
-- Just 1
-- >>> null q
-- True
null :: Deque a -> IO Bool
null Deque{..} = do
  b <- readCounter bottom
  t <- readCounter top
  let sz = b - t
  return (sz <= 0)

-- | Compute a lower and upper bound on the number of elements left in the 'Deque'.
--
-- Under contention from stealing threads or when used by a stealing thread these
-- numbers may well differ.
size :: Deque a -> IO (Int, Int)
size Deque{..} = do
  b1 <- readCounter bottom
  t  <- readCounter top
  b2 <- readCounter bottom
  let sz1 = b1 - t -- always the lower bound on x86, due to lack of load reordering
      sz2 = b2 - t -- always the upper bound on x86, due to lack of load reordering
  return (min sz1 sz2, max sz1 sz2)

-- * Queue Operations

-- | For a work-stealing queue `push` is the ``local'' push.
--
-- Thus only a single thread should perform this operation.
--
-- 'push' and 'pop' together act like a stack.
--
-- >>> q :: Deque String <- empty
-- >>> push "hello" q
-- >>> push "world" q
-- >>> pop q
-- Just "world"
-- >>> pop q
-- Just "hello"
-- >>> pop q
-- Nothing
--
-- 'push' and 'steal' together act like a queue.
--
-- >>> p :: Deque String <- empty
-- >>> push "hello" p
-- >>> push "world" p
-- >>> steal p
-- Just "hello"
-- >>> steal p
-- Just "world"
-- >>> steal p
-- Nothing
push :: a -> Deque a -> IO ()
push obj Deque{..} = do
  b   <- readCounter bottom
  t   <- readCounter top
  arr <- readIORef array
  let len = MV.length arr
      sz = b - t

  arr' <- if sz < len - 1 then return arr else do
    arr' <- growCirc t b arr -- Double in size, don't change b/t.
    -- Only a single thread will do this!:
    writeIORef array arr'
    return arr'

  putCirc arr' b obj
  {-
     KG: we need to put write barrier here since otherwise we might
     end with elem not added to q->elements, but q->bottom already
     modified (write reordering) and with stealWSDeque_ failing
     later when invoked from another thread since it thinks elem is
     there (in case there is just added element in the queue). This
     issue concretely hit me on ARMv7 multi-core CPUs
   -}
  writeBarrier
  writeCounter bottom (b+1)
  return ()

-- TODO: consolidate the writes behind one barrier!
pushes :: Foldable f => f a -> Deque a -> IO ()
pushes objs d = for_ (Reverse objs) $ \a -> push a d
{-# INLINE pushes #-}

-- | This is the work-stealing dequeue operation.
--
-- Multiple threads may concurrently attempt to 'steal' from the same thread.
--
-- It may return 'Nothing' during a lost race with another concurrent reader,
-- so failing to read is /not/ an indication that the 'Deque' is empty!
--
-- However, at least one of the concurrently stealing threads will succeed.
steal :: Deque a -> IO (Maybe a)
steal Deque{..} = do
  -- NB. these loads must be ordered, otherwise there is a race
  -- between steal and pop.
  tt  <- readCounterForCAS top
  loadLoadBarrier
  b   <- readCounter bottom
  arr <- readIORef array
  let t = peekCTicket tt
      sz = b - t
  if sz <= 0 then return Nothing else do
    a <- getCirc arr t
    (b',_) <- casCounter top tt (t+1)
    return $! if b' then Just a
                    else Nothing -- Someone beat us, abort

-- | Locally pop the 'Deque'. This is not a thread safe operation, and should
-- only be invoked on from the thread that \"owns\" the 'Deque'.
pop :: Deque a -> IO (Maybe a)
pop Deque{..} = do
  b0  <- readCounter bottom
  arr <- readIORef array
  b   <- evaluate (b0-1)
  writeCounter bottom b

  -- very important that the following read of q->top does not occur
  -- before the earlier write to q->bottom.
  storeLoadBarrier
  tt   <- readCounterForCAS top
  let t = peekCTicket tt
      sz = b - t
  if sz < 0 then do
    writeCounter bottom t
    return Nothing
   else do
    obj <- getCirc arr b
    if sz > 0 then do
      return (Just obj)
     else do
      (b',_) <- casCounter top tt (t+1)
      writeCounter bottom (t+1)
      return $! if b' then Just obj
                      else Nothing

-- * Circular array routines:

-- TODO: make a "grow" that uses memcpy.
growCirc :: Int -> Int -> MVector RealWorld a -> IO (MVector RealWorld a)
growCirc s e old = do
  let len = MV.length old
  new <- MV.unsafeNew (len + len)
  forN_ s e $ \i -> do
    x <- getCirc old i
    _ <- evaluate x
    putCirc new i x
  return new
{-# INLINE growCirc #-}

getCirc :: MVector RealWorld a -> Int -> IO a
getCirc arr ind = MV.unsafeRead arr (ind `mod` MV.length arr)
{-# INLINE getCirc #-}

putCirc :: MVector RealWorld a -> Int -> a -> IO ()
putCirc arr ind x = MV.unsafeWrite arr (ind `mod` MV.length arr) x
{-# INLINE putCirc #-}
