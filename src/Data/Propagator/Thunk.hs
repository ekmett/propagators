{-# LANGUAGE LambdaCase #-}

{-|

The 'Thunk' API provides a way to defer potentially recursive computations:

* 'thunk' is lazy in its argument, and does not run it directly
* the fist 'kick' triggers execution of the action passed to thunk
* that action is run at most once
* 'force' triggers execution of the action passed to thunk, and does not return until
  *  the action is executed
  *  the action of any thunk kicked by the action is executed, etc.
* Cycles are allowed: The action passed to 'thunk' may 'kick' that 'thunk'. Same for larger loops.

The implementation is hopefully thread safe: Even if multiple threads force or kick related thunks, all actions are still run at most once, and all calls to force terminate (no deadlock)
-}
module Data.Propagator.Thunk
    ( Thunk
    , KickedThunk
    , thunk
    , doneThunk
    , kick
    , force
    )
where

import Control.Concurrent

newtype Thunk = Thunk (MVar (Either (IO [KickedThunk]) KickedThunk))
data ResolvingState = NotStarted | ProcessedBy ThreadId (MVar ()) | Done
data KickedThunk = KickedThunk (MVar [KickedThunk]) (MVar ResolvingState)

thunk :: IO [KickedThunk] -> IO Thunk
thunk act = Thunk <$> newMVar (Left act)

doneThunk :: IO Thunk
doneThunk = do
    mv_ts <- newMVar []
    mv_s <- newMVar Done
    Thunk <$> newMVar (Right (KickedThunk mv_ts mv_s))

-- Recursively explores the thunk, and kicks the execution
-- May return before before execution is done (if started by another thread)
kick :: Thunk -> IO KickedThunk
kick (Thunk t) = takeMVar t >>= \case
    Left act -> do
        mv_thunks <- newEmptyMVar
        mv_state <- newMVar NotStarted
        let kt = KickedThunk mv_thunks mv_state
        putMVar t (Right kt)

        kts <- act
        putMVar mv_thunks kts
        pure kt

    -- Thread was already kicked, nothing to do
    Right kt -> do
        putMVar t (Right kt)
        pure kt

wait :: KickedThunk -> IO ()
wait (KickedThunk mv_deps mv_s) = do
    my_id <- myThreadId
    s <- takeMVar mv_s
    case s of
        -- Thunk and all dependences are done
        Done -> putMVar mv_s s
        -- Thunk is being processed by a higher priority thread, so simply wait
        ProcessedBy other_id done_mv | other_id < my_id -> do
            putMVar mv_s s
            readMVar done_mv
        -- Thunk is already being processed by this thread, ignore
        ProcessedBy other_id _done_mv | other_id == my_id -> do
            putMVar mv_s s
            pure ()
        -- Thunk is not yet processed, or processed by a lower priority thread, so process now
        _ -> do
            done_mv <- newEmptyMVar
            putMVar mv_s (ProcessedBy my_id done_mv)
            ts <- readMVar mv_deps
            mapM_ wait ts
            -- Mark kicked thunk as done
            _ <- swapMVar mv_s Done
            -- Wake up waiting threads
            putMVar done_mv ()

force :: Thunk -> IO ()
force t = do
    rt <- kick t
    wait rt
