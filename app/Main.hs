module Main where

import Data.Time.Clock ( diffUTCTime, getCurrentTime )
import Lib ( readLines, quickSeq, mergeSeq, bitonicSeq, shuffle )
import Data.List (sort)
-- import qualified Data.ByteString as B
import qualified Data.Vector as V
import Control.DeepSeq ( force )
import Data.String ( fromString )

main :: IO ()
main = do 
    -- v <- readLines "res/shuffledwords.txt"
    v <- shuffle $ V.enumFromTo 0 (10^6::Integer)
    time "quickSort" (quickSeq v)
    time "sort" $ sort (V.toList v)
    time "mergeSort" (mergeSeq v) 
    -- time "bitonicsort" (bitonicSeq (fromString "zz") v) 
    time "bitonicsort" (bitonicSeq (10^6) v) 
    where
    time msg a = do
        start <- getCurrentTime
        let a' = force a
        end <- a' `seq` getCurrentTime
        putStrLn $ msg ++ ": " ++ show (diffUTCTime end start)
