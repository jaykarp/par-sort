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
    o <- M.new (V.length x)
    runs' 1 x 0 o  
    where
        runs' i x k o
            | i < V.length x = 
                if x!(i-1) <= x!i then 
                    asc (i-1) i k o
                else 
                    dsc (i-1) i k o
            | otherwise = return $ M.slice 0 k o 
        asc s i k o = 
            if i < V.length x && x!(i-1) <= x!i then 
                asc s (i+1) k o
            else do
                M.write o k (V.slice s (i-s) x)
                runs' (i+1) x (k+1) o
        dsc s i k o = 
            if i < V.length x && x!(i-1) > x!i then
                dsc s (i+1) k o
            else do
                M.write o k (V.reverse $ V.slice s (i-s) x)
                runs' (i+1) x (k+1) o

merge :: Ord a => V.Vector (V.Vector a) -> V.Vector a
merge v = (!0) $ V.create $ do
    o <- V.thaw v 
    mergeAll (V.length v) o
    return $ M.slice 0 1 o
    where
        mergeAll k o 
            | k == 1 = return ()  
            | otherwise = do
                k' <- mergePairs 0 k o 
                mergeAll k' o
        mergePairs i k o
            | i < k - 1 = do
                oi <- M.read o i 
                oip1 <- M.read o (i+1)
                M.write o (i `div` 2) (merge2 oi oip1)
                mergePairs (i+2) k o
            | i == k - 1 = do
                oi <- M.read o i
                M.write o (i `div` 2) oi
                return $ k `div` 2 + 1 
            | otherwise = return $ k `div` 2

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


