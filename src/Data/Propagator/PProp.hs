{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
-- | An alternative to "Data.Propagator.Prop".
module Data.Propagator.PProp where

import System.IO.Unsafe
import Control.Monad.ST
import Data.Monoid
import Data.Coerce

import Data.Propagator.Class
import Data.Propagator.Cell
import Data.Propagator.Thunk

-- At least for my example, I do not want empty cells (because (&&) can
-- propagate information even if one argument is still unknown) so lets
-- only work with lattices with bottom
class Propagated a => HasBottom a where
    bottom :: a
instance (Eq a, Monoid a) =>  HasBottom (MergeUsingSemigroup a) where
    bottom = MergeUsingSemigroup mempty
deriving via MergeUsingSemigroup Any instance HasBottom Any
deriving via MergeUsingSemigroup All instance HasBottom All

data PProp a where
    PProp :: Cell RealWorld a -> Thunk -> PProp a

defineConst :: HasBottom a => a -> PProp a
defineConst x = unsafePerformIO $ do
    c <- stToIO $ known x
    t <- doneThunk
    pure (PProp c t)

defineUnary :: HasBottom b => (a -> b) -> PProp a -> PProp b
defineUnary f p = unsafePerformIO $ do
    c <- stToIO $ known bottom
    t <- thunk $ do
        -- NB: Only peek at p inside thunk
        let PProp c1 t1 = p
        stToIO $ lift1 f c1 c
        mapM kick [t1]
    pure (PProp c t)

defineBinary :: HasBottom c => (a -> b -> c) -> PProp a -> PProp b -> PProp c
defineBinary f p1 p2 = unsafePerformIO $ do
    c <- stToIO $ known bottom
    t <- thunk $ do
        let PProp c1 t1 = p1
        let PProp c2 t2 = p2
        stToIO $ lift2 f c1 c2 c
        mapM kick [t1, t2]
    pure (PProp c t)

-- could also do a
--   defineNary :: Propagated c => ([a] -> c) -> [PProp a] -> PProp c
-- if we add a liftN in Cell.hs

getPProp :: PProp a -> a
getPProp (PProp c t) = unsafePerformIO $ do
    force t
    m <- stToIO $ content c
    case m of
        Nothing -> error "Empty cell in PProp"
        Just x -> pure x


-- A little demo with the two point lattice True < False


type PAll = PProp All
pTrue :: PAll
pTrue = defineConst (All True)
pFalse :: PAll
pFalse = defineConst (All False)

(&&&) :: PAll -> PAll -> PAll
(&&&) = defineBinary (coerce (&&))

pand :: [PAll] -> PAll
pand = foldr (&&&) pTrue

getPAll :: PAll -> Bool
getPAll = getAll . getPProp

examples :: [Bool]
examples =
  [ (let x = pand [y]; y = pand [x, pFalse] in getPAll x) == False
  , (let x = pand [y]; y = pand [x, pTrue]  in getPAll x) == True
  , (let x = pand [y]; y = pand [x]         in getPAll x) == True
  ]
