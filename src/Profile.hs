{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators      #-}

import           Control.Monad.Identity (runIdentity)
import qualified Data.Array.Repa        as R
import           Life
import           System.Console.CmdArgs

data Profile = Profile { width_       :: Int
                       , height_      :: Int
                       , generations_ :: Int }
                       deriving (Show, Data, Typeable)

argsProfile :: Profile
argsProfile = Profile
                { width_        =  200  &= help "The number of cells across the game board."
                , height_       =  200  &= help "The number of cells tall."
                , generations_  =  100  &= help "The number of generations to calculate." }

main :: IO ()
main = do
        profile  <- cmdArgs argsProfile
        firstGen <- makeFirstGen profile
        let finalAlive = simulate (generations_ profile) firstGen
        putStrLn $ "Final Alive: " ++ show finalAlive

makeFirstGen :: Profile -> IO Generation
makeFirstGen (Profile w h _) = randomGen w h

simulate :: Int -> Generation -> Int
simulate 0 = runIdentity . R.sumAllP . R.map fromIntegral
simulate i = simulate (i - 1) . nextGen
