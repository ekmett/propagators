module Model.Log
  ( 
  -- * Log
    Log
  , newLog
  , record

  -- * Cursors
  , Cursor
  , newCursor
  , oldCursor
  , advance
  , deleteCursor
  , validCursor

  -- * Exceptions
  , InvalidCursor
  ) where

import Model.Internal.Log
