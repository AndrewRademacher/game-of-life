{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}

import           Prelude                        as P
import           Life
import           System.Console.CmdArgs
import           System.Random
import           Data.Text                      ()
import           Data.Text.IO                   as TIO
import           Data.Text.Format               as TF

data Profile = Profile { width_       :: Int
                       , height_      :: Int
                       , generations_ :: Int }
                       deriving (Show, Data, Typeable)

argsProfile :: Profile
argsProfile = Profile
                { width_        =  50  &= help "The number of cells across the game board."
                , height_       =  50  &= help "The number of cells tall."
                , generations_  =  100 &= help "The number of generations to calculate." }

main :: IO ()
main = do
        profile <- cmdArgs argsProfile
        seed <- newStdGen
        let firstGen   = makeFirstGen profile seed
            finalAlive = simulate (generations_ profile) firstGen
        TF.print "Final Alive: {}\n" $ TF.Only finalAlive

makeFirstGen :: Profile -> StdGen -> Generation
makeFirstGen (Profile w h g) seed =
        Generation w h 1 1 (randomBoard w h seed)

simulate :: Int -> Generation -> Int
simulate genCount currGen = undefined
