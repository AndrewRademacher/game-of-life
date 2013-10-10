{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}

import           Prelude                                as P
import           System.Console.CmdArgs
import           Data.Text                              as T
import           Data.Text.IO                           as TIO
import           Data.Array.Repa                        as R
import           Data.Array.Repa.Algorithms.Randomish   as AR
import           Graphics.Gloss

type Board = Array U DIM2 Int
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
        let board         = randomishIntArray (Z :. (width life) :. (height life)) 0 1 1
            displayWidth  = (cellWidth life) * (width life)
            displayHeight = (cellWidth life) * (height life)
        display (InWindow "Game of Life" (displayWidth, displayHeight) (10, 10))
                white (pictureBoard life board)

pictureBoard :: Life -> Board -> Picture
pictureBoard life board = Scale 20 20
                        $ Polygon [(0, 0), (0, 1), (1, 1), (1, 0)]
