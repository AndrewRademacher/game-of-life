{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}

import           Prelude                                as P
import           System.Random
import           System.Console.CmdArgs
import           Data.Text                              as T
import           Data.Text.IO                           as TIO
import           Data.List                              as L
import           Data.Vector                            as V
import           Data.Vector.Unboxed                    as U
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort

data Life = Life { width_     :: Int
                 , height_    :: Int
                 , cellWidth_ :: Int
                 , genPerSec_ :: Int }
            deriving (Show, Data, Typeable)

argsLife = Life { width_         = 50  &= help "The number of cells across the game board."
                , height_        = 50  &= help "The number of cells tall."
                , cellWidth_     = 10  &= help "The number of pixels across a single cell."
                , genPerSec_     = 1   &= help "The number of generations per second." }

type Board = V.Vector Int
data Generation = Generation { width     :: Int
                             , height    :: Int
                             , cellWidth :: Float
                             , genPerSec :: Int
                             , board     :: Board }

main :: IO ()
main = do
        life <- cmdArgs argsLife
        seed <- newStdGen
        let firstGen      = makeFirstGen life seed
            displayWidth  = (cellWidth_ life) * (width_ life)
            displayHeight = (cellWidth_ life) * (height_ life)
        simulate (InWindow "Game of Life" (displayWidth, displayHeight) (10, 10))
            white (genPerSec firstGen) firstGen pictureGeneration nextGeneration

makeFirstGen :: Life -> StdGen -> Generation
makeFirstGen (Life w h cw gps) seed = 
        Generation w h (fromIntegral cw :: Float) gps (randomBoard w h seed)

randomBoard :: Int -> Int -> StdGen -> Board
randomBoard w h = V.take (w * h) . V.unfoldr (Just . randomR (0, 1))

getCoords :: Generation -> Int -> (Int, Int)
getCoords gen idx = (x, y)
    where x  = idx `mod` (width gen)
          y  = idx `div` (width gen)

fromCoords :: Generation -> (Int, Int) -> Int
fromCoords gen (x, y) = x + (y * (width gen))

pictureGeneration :: Generation -> Picture
pictureGeneration gen   = Translate tx ty
                        $ Scale (cellWidth gen) (cellWidth gen)
                        $ Pictures (V.toList (V.imap (pictureCell gen) (board gen)))
    where tx = 0 - (((cellWidth gen) * w) / 2)
          ty = 0 - (((cellWidth gen) * h) / 2)
          w  = (fromIntegral (width gen) :: Float)
          h  = (fromIntegral (height gen) :: Float)

pictureCell :: Generation -> Int -> Int -> Picture
pictureCell gen idx state  = Translate (fromIntegral x :: Float) (fromIntegral y :: Float)
                            $ Color (color state)
                            $ Polygon [(0, 0), (0, 1), (1, 1), (1, 0)]
    where (x, y)  = getCoords gen idx
          color 0 = makeColor 1 1 1 1
          color 1 = makeColor 0 0 0 1

nextGeneration :: ViewPort -> Float -> Generation -> Generation
nextGeneration view time gen@(Generation w h cw gps board) = 
        Generation w h cw gps (V.imap (nextCell gen) board)

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
