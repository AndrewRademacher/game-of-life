{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

import           Control.Monad.Identity       (runIdentity)
import           Data.Array.Repa              as R
import           Data.Array.Repa.Repr.Vector
import           Data.Text                    ()
import           Data.Text.IO                 ()
import           Data.Word
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Life
import           Prelude                      as P
import           System.Console.CmdArgs

data Life = Life
          { width_     :: Int
          , height_    :: Int
          , cellWidth_ :: Int
          , genPerSec_ :: Int }
          deriving (Show, Data, Typeable)

data World = World
           { board     :: !Generation
           , width     :: !Int
           , height    :: !Int
           , cellWidth :: !Float }

argsLife :: Life
argsLife = Life { width_         = 50  &= help "The number of cells across the game board."
                , height_        = 50  &= help "The number of cells tall."
                , cellWidth_     = 10  &= help "The number of pixels across a single cell."
                , genPerSec_     = 1   &= help "The number of generations per second." }

main :: IO ()
main = do
        life     <- cmdArgs argsLife
        firstGen <- randomGen (width_ life) (height_ life)
        let firstWorld = World
                       { board     = firstGen
                       , width     = width_ life
                       , height    = height_ life
                       , cellWidth = fromIntegral (cellWidth_ life) :: Float }
            displayW   = (cellWidth_ life) * (width_ life)
            displayH   = (cellWidth_ life) * (height_ life)
        simulate (InWindow "Game of Life" (displayW, displayH) (10, 10))
            white (genPerSec_ life) firstWorld pictureWorld nextWorld

nextWorld :: ViewPort -> Float -> World -> World
nextWorld _ _ world = world { board = nextGen (board world) }

pictureWorld :: World -> Picture
pictureWorld world = Translate tx ty
                   $ Scale (cellWidth world) (cellWidth world)
                   $ Pictures (R.toList $ picGen (board world))
    where picGen :: Generation -> Array V DIM2 Picture
          picGen gen = runIdentity . computeP $ traverse gen id pictureCell
          tx         = 0 - (((cellWidth world) * w) / 2)
          ty         = 0 - (((cellWidth world) * h) / 2)
          w          = fromIntegral (width world) :: Float
          h          = fromIntegral (height world) :: Float

pictureCell :: (DIM2 -> Word8) -> DIM2 -> Picture
pictureCell lkp loc@(Z :. x :. y) = Translate fx fy
                                  $ Color (stateColor $ lkp loc)
                                  $ Polygon [(0.1, 0.1), (0.1, 0.9), (0.9, 0.9), (0.9, 0.1)]
    where fx = fromIntegral x :: Float
          fy = fromIntegral y :: Float
          stateColor 0  = makeColor 0.9 0.9 0.9 1
          stateColor 1  = makeColor 0 0 0 1
          stateColor _  = makeColor 0 0 0 0
