module Main where

import Data.Time.Clock
import Lib ( readLines,
             mergeSort,
           )
import Data.List (sort)
-- import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Vector ((!))

main :: IO ()
main = do 
   v <- readLines "res/shuffledwords2.txt"
   start <- getCurrentTime
   print $ mergeSort v!0
   end <- getCurrentTime
--    start2 <- getCurrentTime
--    print $ sort $ V.toList v 
--    end2 <- getCurrentTime
   print $ diffUTCTime end start
--    print $ diffUTCTime end2 start2