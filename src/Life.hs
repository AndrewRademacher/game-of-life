{-# LANGUAGE BangPatterns #-}

module Life
    ( Generation
    , PreGeneration

    , randomGen
    , nextGen
    ) where

import           Control.Monad.Identity               (runIdentity)
import           Data.Array.Repa                      as R
import           Data.Array.Repa.Algorithms.Randomish as RA
import           Data.Word
import           Prelude                              hiding (map)
import           System.Random

type PreGeneration = Array U DIM2 Int
type Generation    = Array U DIM2 Word8

randomGen :: Int -> Int -> StdGen -> Generation
randomGen w h g = runIdentity . computeP $ map fromIntegral $ randomishIntArray (Z :. w :. h :: DIM2) 0 1 s
    where (s, _) = (random g :: (Int, StdGen))

nextGen :: Generation -> Generation
nextGen !lastGen = runIdentity . computeP $ traverse lastGen id (nextCell (extent lastGen))

nextCell :: DIM2 -> (DIM2 -> Word8) -> DIM2 -> Word8
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
