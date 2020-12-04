module Main where

import Data.Time.Clock
import Lib ( readLines,
             mergeSort,
           )
-- import qualified Data.ByteString as B
-- import qualified Data.Vector as V
import Data.Vector ((!))

main :: IO ()
main = do 
   v <- readLines "res/shuffledwords.txt"
   start <- getCurrentTime
   print $ mergeSort v!0
   end <- getCurrentTime
   print $ diffUTCTime end start