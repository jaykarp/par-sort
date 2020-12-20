{-# Language DeriveGeneric #-} 
module Main where

import Data.Time.Clock ( diffUTCTime, getCurrentTime, NominalDiffTime )
import Lib ( readLines, quickSeq, mergeSeq, bitonicSeq, shuffle, mergePar, bitonicPar, merge )
import Data.List (sort)
import qualified Data.Vector as V
import Control.DeepSeq ( NFData, force )
import Data.String ( fromString )
import GHC.Generics (Generic)
newtype Dumb = Dumb (Integer) deriving (Generic, Show)

instance Eq (Dumb) where
    (Dumb 0) == (Dumb 0) = True
    (Dumb x) == (Dumb 0) = False
    (Dumb 0) == (Dumb y) = False 
    (Dumb x) == (Dumb y) = Dumb (x-1) == Dumb (y-1)

instance Ord (Dumb) where
    (Dumb x) <= (Dumb 0) = x <= 0 
    (Dumb 0) <= (Dumb y) = y >= 0 
    (Dumb x) <= (Dumb y) = (Dumb (x-1)) <= (Dumb (y-1)) 

instance Num (Dumb) where
    (+) (Dumb a) (Dumb b) = Dumb $ a + b
    (*) (Dumb a) (Dumb b) = Dumb $ a * b
    abs (Dumb a) = Dumb $ abs a
    fromInteger = Dumb
    negate (Dumb a) = Dumb $ -a
    signum (Dumb a) = Dumb $ signum a

instance NFData (Dumb)

main :: IO ()
main = do 
    -- v <- readLines "res/shuffledwords.txt"
    v <- shuffle $ V.enumFromN (10^4 :: Dumb) (10^3)
    s <- time "mergeSeq" (mergeSeq v) 
    p <- time "mergePar" (mergePar v) 
    print $ s / p
    print $ V.take 10 $ (mergePar v) 



time :: (NFData a) => String -> a -> IO NominalDiffTime
time msg a = do
    start <- getCurrentTime
    let a' = force a
    end <- a' `seq` getCurrentTime
    putStrLn $ msg ++ ": " ++ show (diffUTCTime end start)
    return $ diffUTCTime end start

timeIO msg io = do
    start <- getCurrentTime
    a <- io
    end <- (force a) `seq` getCurrentTime
    putStrLn $ msg ++ ": " ++ show (diffUTCTime end start)
