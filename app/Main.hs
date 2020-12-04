module Main where

import Lib ( readLines,
             mergeSort,
           )
-- import qualified Data.ByteString as B
-- import qualified Data.Vector as V
import Data.Vector ((!))

main :: IO ()
main = do 
   v <- readLines "res/shuffledwords.txt"
--    let v = V.reverse $ V.fromList [0..1000000]
   print $ mergeSort v!0