{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Model.Internal.Util where

import Data.Primitive.Array
import GHC.Exts as Exts

sizeofMutableArray :: MutableArray s a -> Int
sizeofMutableArray (MutableArray s) = I# (Exts.sizeofMutableArray# s)

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
forN_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
forN_ start end _ | start > end = error "for_: start is greater than end"
forN_ start end fn = loop start
  where
   loop !i | i == end  = return ()
           | otherwise = do fn i; loop (i+1)
{-# INLINE forN_ #-}
