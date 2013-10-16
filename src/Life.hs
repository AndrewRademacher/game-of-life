{-# LANGUAGE BangPatterns #-}

module Life
    ( Generation

    , randomGen
    , nextGen
    ) where

import           System.Random
import           Control.Monad.Identity (runIdentity)
import           Data.Array.Repa                        as R
import           Data.Array.Repa.Algorithms.Randomish   as RA

type Generation = Array U DIM2 Int

randomGen :: Int -> Int -> StdGen -> Generation
randomGen width height gen = randomishIntArray (Z :. width :. height :: DIM2) 0 1 s
    where (s, _) = (random gen :: (Int, StdGen))

nextGen :: Generation -> Generation
nextGen !lastGen = runIdentity . computeP $ traverse lastGen id (nextCell (extent lastGen))

nextCell :: DIM2 -> (DIM2 -> Int) -> DIM2 -> Int
{-# INLINE nextCell #-}
nextCell !aSh !lkp !loc@(Z :. x :. y) |  nc < 2 || nc > 3  =  0
                                      |  nc == 3           =  1
                                      |  otherwise         =  lkp loc
    where !nc       =   (gn n n) + (gn z n) + (gn p n)
                      + (gn n z) +            (gn p z)
                      + (gn n p) + (gn z p) + (gn p p)
          gn !mx !my | inShape aSh nLoc = lkp nLoc
                     | otherwise        = 0
            where !nLoc = (Z :. x + mx :. y + my)
          !n       = (-1)
          !p       = (1)
          !z       = (0)
