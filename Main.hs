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

type Board = V.Vector Int
data Life = Life { width     :: Int
                 , height    :: Int
                 , cellWidth :: Int
                 , genDelay  :: Float }
            deriving (Show, Data, Typeable)

argsLife = Life { width         = 50  &= help "The number of cells across the game board."
                , height        = 50  &= help "The number of cells tall."
                , cellWidth     = 10  &= help "The number of pixels across a single cell."
                , genDelay      = 1   &= help "The number of seconds between each generation." }

main :: IO ()
main = do
        life <- cmdArgs argsLife
        seed <- newStdGen
        let board = randomBoard (width life) (height life) seed
            displayWidth  = (cellWidth life) * (width life)
            displayHeight = (cellWidth life) * (height life)
        display (InWindow "Game of Life" (displayWidth, displayHeight) (10, 10))
                white (pictureBoard life board displayWidth displayHeight)

randomBoard :: Int -> Int -> StdGen -> Board
randomBoard w h = V.take (w * h) . V.unfoldr (Just . randomR (0, 1))

getCoords :: Life -> Int -> (Int, Int)
getCoords life idx = (x, y)
    where x  = idx `mod` (width life)
          y  = idx `div` (width life)

fromCoords :: Life -> (Int, Int) -> Int
fromCoords life (x, y) = x + (y * (width life))

pictureBoard :: Life -> Board -> Int -> Int -> Picture
pictureBoard life board dw dh   = Translate tx ty
                                $ Scale cw cw 
                                $ Pictures (V.toList (V.imap (pictureCell life) board))
    where cw = (fromIntegral (cellWidth life) :: Float)
          tx = (fromIntegral (0 - (dw `div` 2)) :: Float)
          ty = (fromIntegral (0 - (dh `div` 2)) :: Float)

pictureCell :: Life -> Int -> Int -> Picture
pictureCell life idx state  = Translate (fromIntegral x :: Float) (fromIntegral y :: Float)
                            $ Color (color state)
                            $ Polygon [(0, 0), (0, 1), (1, 1), (1, 0)]
    where (x, y)  = getCoords life idx
          color 0 = makeColor 1 1 1 1
          color 1 = makeColor 0 0 0 1
