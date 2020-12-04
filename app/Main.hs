module Main where

import System.CPUTime
import Lib ( readLines,
             mergeSort,
           )
-- import qualified Data.ByteString as B
-- import qualified Data.Vector as V
import Data.Vector ((!))

main :: IO ()
main = do 
   v <- readLines "res/shuffledwords.txt"
   start <- getCPUTime
   print $ mergeSort v!0
   end <- getCPUTime
   let diff = (fromIntegral (end - start)) / (10^12)
   print diff