module Lib
    ( readLines,
      mergeSort,
      runs,
      merge,
      merge2,
    ) where

import System.IO (withFile, IOMode(ReadMode), hIsEOF)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Data.Vector (MVector, (!))
import Control.Monad.ST (ST)

mergeSort :: Ord a => V.Vector a -> V.Vector a
mergeSort = merge . runs

runs :: Ord a => V.Vector a -> V.Vector (V.Vector a)
runs x = V.create $ do
    v <- M.new (V.length x)
    go 1 0 0 v
    where
        go i j k v
            | i < V.length x = do
                if x!(i-1) > x!i then do
                    M.write v k (V.slice j (i-j) x)
                    go (i+1) i (k+1) v
                else go (i+1) j k v
            | otherwise = do
                M.write v k (V.slice j (i-j) x)
                return $ M.slice 0 k v

merge :: Ord a => V.Vector (V.Vector a) -> V.Vector a
merge v
    | V.length v == 0 = V.empty
    | V.length v == 1 = v!0
    | otherwise = merge2 (merge a) (merge b) 
    where
        (a, b) = V.splitAt (V.length v `div` 2) v

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


