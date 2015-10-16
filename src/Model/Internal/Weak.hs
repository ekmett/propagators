{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Model.Internal.Weak where

import GHC.Conc.Sync
import GHC.IO
import GHC.Prim
import GHC.Weak

-- | Attach a finalizer directly to the internal 'ThreadId#' and create
-- a @'Weak' 'ThreadId'@. Normally, keeping a reference to a 'ThreadId'
-- alive will also keep the thread itself around.
mkWeakThreadId :: ThreadId -> IO () -> IO (Weak ThreadId)
mkWeakThreadId t@(ThreadId t#) f = IO $ \s -> case mkWeak# t# t f s of
  (# s1, w #) -> (# s1, Weak w #)
