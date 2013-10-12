{-# LANGUAGE BangPatterns #-}

module Life
    ( Board
    , Generation (..)

    , randomBoard
    , getCoords
    , fromCoords
    , countAlive
    , nextGeneration
    , nextCell
    ) where

import           System.Random
import           Data.List                          as L
import           Data.Vector                        as V
import           Data.Vector.Unboxed                as U

type Board = U.Vector Int
data Generation = Generation { width     :: Int
                             , height    :: Int
                             , cellWidth :: Float
                             , genPerSec :: Int
                             , board     :: Board }

randomBoard :: Int -> Int -> StdGen -> Board
randomBoard w h = U.take (w * h) . U.unfoldr (Just . randomR (0, 1))

getCoords :: Generation -> Int -> (Int, Int)
getCoords !gen !idx = (x, y)
    where !x  = idx `mod` (width gen)
          !y  = idx `div` (width gen)

fromCoords :: Generation -> (Int, Int) -> Int
fromCoords !gen (!x, !y) = x + (y * (width gen))

countAlive :: Generation -> Int
countAlive (Generation _ _ _ _ bord) = U.sum bord

nextGeneration :: Generation -> Generation
nextGeneration gen@(Generation w h cw gps bord) = 
        Generation w h cw gps (U.imap (nextCell gen) bord)

nextCell :: Generation -> Int -> Int -> Int
nextCell gen idx state | nc < 2 || nc > 3   = 0
                       | nc == 3            = 1
                       | otherwise          = state
    where (!x, !y)      = getCoords gen idx
          !nc           =         (gn neg neg) + (gn zro neg)   + (gn pos neg)
                                + (gn neg zro) + (0)            + (gn pos zro)
                                + (gn neg pos) + (gn zro pos)   + (gn pos pos)  
          gn !mx !my    | midx < 0                          = 0
                        | midx >= (U.length (board gen))    = 0
                        | otherwise                         = (board gen) U.! midx
                where !midx = fromCoords gen (x + mx, y + my)
          neg           = (-1)
          pos           = (1)
          zro           = (0)
