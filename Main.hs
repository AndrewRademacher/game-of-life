{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}

import           Prelude                                as P
import           System.Console.CmdArgs
import           Data.Text                              as T
import           Data.Text.IO                           as TIO
import           Data.Array.Repa                        as R
import           Data.Array.Repa.Algorithms.Randomish   as Ar

data Life = Life { boardWidth  :: Int
                 , cellWidth   :: Int
                 , generations :: Int }
            deriving (Show, Data, Typeable)

argsLife = Life { boardWidth    = 50  &= help "The number of cells across the game board."
                , cellWidth     = 10  &= help "The number of pixels across a single cell."
                , generations   = 100 &= help "The number of generations to simulate." }

main :: IO ()
--main = P.print =<< cmdArgs argsLife
main = do
        args <- cmdArgs argsLife
        P.print args
