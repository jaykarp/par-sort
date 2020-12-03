module Main where

import Lib
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Vector ((!))

main :: IO ()
main = do 
   v <- readLines "res/words.txt"
   print $ v!0 
