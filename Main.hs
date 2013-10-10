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
        P.print life
        P.print board
--        display (InWindow "Game of Life" (displayWidth, displayHeight) (10, 10))
--                white (pictureBoard life)

randomBoard :: Int -> Int -> StdGen -> U.Vector Int
randomBoard w h = U.take (w * h) . U.unfoldr (Just . randomR (0, 1))

pictureBoard :: Life -> Picture
pictureBoard life = Scale 20 20
                  $ Polygon [(0, 0), (0, 1), (1, 1), (1, 0)]
