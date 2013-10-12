
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

type Board = V.Vector Int
data Generation = Generation { width     :: Int
                             , height    :: Int
                             , cellWidth :: Float
                             , genPerSec :: Int
                             , board     :: Board }

randomBoard :: Int -> Int -> StdGen -> Board
randomBoard w h = V.take (w * h) . V.unfoldr (Just . randomR (0, 1))

getCoords :: Generation -> Int -> (Int, Int)
getCoords gen idx = (x, y)
    where x  = idx `mod` (width gen)
          y  = idx `div` (width gen)

fromCoords :: Generation -> (Int, Int) -> Int
fromCoords gen (x, y) = x + (y * (width gen))

countAlive :: Generation -> Int
countAlive (Generation _ _ _ _ bord) = V.sum bord

nextGeneration :: Generation -> Generation
nextGeneration gen@(Generation w h cw gps bord) = 
        Generation w h cw gps (V.imap (nextCell gen) bord)

nextCell :: Generation -> Int -> Int -> Int
nextCell gen idx state | nc < 2 || nc > 3   = 0
                       | nc == 3            = 1
                       | otherwise          = state
    where (x, y)        = getCoords gen idx
          nc            = L.sum [ (gn neg neg) , (gn zro neg)   , (gn pos neg)
                                , (gn neg zro) , (0)            , (gn pos zro)
                                , (gn neg pos) , (gn zro pos)   , (gn pos pos) ]
          gn mx my      | midx < 0                          = 0
                        | midx >= (V.length (board gen))    = 0
                        | otherwise                         = (board gen) V.! midx
                where midx = fromCoords gen (x + mx, y + my)
          neg           = (-1)
          pos           = (1)
          zro           = (0)
