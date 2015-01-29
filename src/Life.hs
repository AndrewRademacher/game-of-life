{-# LANGUAGE QuasiQuotes #-}

module Life
    ( Generation

    , randomGen
    , nextGen
    ) where

import           Control.Monad.Identity       (runIdentity)
import           Data.Array.Repa              as R
import           Data.Array.Repa.Stencil
import           Data.Array.Repa.Stencil.Dim2
import           Data.Vector.Unboxed          (Vector)
import           Data.Word
import           Prelude                      hiding (map)
import           System.Random.MWC

type Generation = Array U DIM2 Word8

randomGen :: Int -> Int -> IO Generation
randomGen w h = do
        vs <- withSystemRandom . asGenST $ \gen -> uniformVector gen (w * h)
        let bs = fromUnboxed (Z :. w :. h :: DIM2) (vs :: Vector Bool)
        return $ runIdentity . computeP $ map (\a -> if a then 1 else 0) bs

nextGen :: Generation -> Generation
nextGen lg = runIdentity . computeP $ R.zipWith nextCell lg ncs
    where ncs = mapStencil2 (BoundConst 0) st lg
          st  = [stencil2| 1 1 1
                           1 0 1
                           1 1 1 |]

nextCell :: (Ord b, Num b, Num a) => a -> b -> a
nextCell lc nc | nc < 2 || nc > 3 = 0
               | nc == 3          = 1
               | otherwise        = lc
