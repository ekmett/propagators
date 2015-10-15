{-# LANGUAGE MagicHash #-}
module Model.Array where

import Data.Primitive.Array
import GHC.Exts as Exts
import GHC.Types

sizeofMutableArray :: MutableArray s a -> Int
sizeofMutableArray (MutableArray s) = I# (Exts.sizeofMutableArray# s)

