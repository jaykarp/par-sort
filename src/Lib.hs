module Lib
    ( readLines,
      runs,
      merge2,
    ) where

import System.IO (withFile, IOMode(ReadMode), hIsEOF)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Data.Vector ((!))

runs :: Ord a => V.Vector a -> [V.Vector a] 
runs = go 1 0
    where
        go i j v 
            | i < V.length v = if v!(i-1) > v!i then V.slice j (i-j) v : go (i + 1) i v else go (i+1) j v
            | otherwise = [V.slice j (i-j) v]

merge2 :: Ord a => V.Vector a -> V.Vector a -> V.Vector a       
merge2 a b = V.create $ do
    v <- M.new (V.length a + V.length b)
    go 0 0 v
    return v
        where go i j v 
                | i < V.length a && j < V.length b = 
                    if a!i <= b!j then do
                        M.write v (i+j) (a!i) 
                        go (i+1) j v
                    else do
                        M.write v (i+j) (b!j)
                        go i (j+1) v
                | i < V.length a = do 
                    M.write v (i+j) (a!i)
                    go (i+1) j v
                | j < V.length b = do 
                    M.write v (i+j) (b!j)
                    go i (j+1) v
                | otherwise = return ()



readLines :: String -> IO (V.Vector B.ByteString)
readLines filename = withFile filename ReadMode ((V.fromList <$>) . getLines)
    where
    getLines handle = do
        eof <- hIsEOF handle  
        if eof then 
            return [] 
        else
            (:) <$> B.hGetLine handle <*> getLines handle 


