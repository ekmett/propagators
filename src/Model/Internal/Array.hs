{-# LANGUAGE MagicHash #-}
module Model.Internal.Array where

import Data.Primitive.Array
import GHC.Exts as Exts

sizeofMutableArray :: MutableArray s a -> Int
sizeofMutableArray (MutableArray s) = I# (Exts.sizeofMutableArray# s)

