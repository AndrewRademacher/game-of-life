{-# LANGUAGE BangPatterns #-}

module Life
    ( Generation

    , randomGen
    , nextGen
    ) where

import           Control.Monad.Identity (runIdentity)
import           Data.Array.Repa        as R
import           Data.Vector.Unboxed    (Vector)
import           Data.Word
import           Prelude                hiding (map)
import           System.Random.MWC

type Generation    = Array U DIM2 Word8

randomGen :: Int -> Int -> IO Generation
randomGen w h = do
        vs <- withSystemRandom . asGenST $ \gen -> uniformVector gen (w * h)
        let bs = fromUnboxed (Z :. w :. h :: DIM2) (vs :: Vector Bool)
        return $ runIdentity . computeP $ map (\a -> if a then 1 else 0) bs

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
