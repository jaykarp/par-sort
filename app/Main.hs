module Main where

import Lib ( readLines )
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Vector ((!))



main :: IO ()
main = do 
   v <- readLines "res/shuffledwords.txt"
   print $ v!0