module Sequential (bitonicSeq, mergeSeq, quickSeq) where

import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Control.Monad (when)
import Utils ( fillBitonic )

bitonicSeq :: Ord a => a -> V.Vector a -> V.Vector a
bitonicSeq = (bitonic .) . fillBitonic

bitonic :: Ord a => V.Vector a -> V.Vector a 
bitonic v = V.create $ do
    o <- V.thaw v 
    bitonicSort' o 0 (V.length v) True 
    return o
    where 
        bitonicSort' o low cnt dir = 
            when (cnt > 1) $ do 
                let k = cnt `div` 2
                bitonicSort' o low k True
                bitonicSort' o (low + k) k False
                bitonicMerge o low cnt dir
        bitonicMerge o low cnt dir =
            when (cnt > 1) $ do
                let k = cnt `div` 2
                loopSwap o low low k dir
                bitonicMerge o low k dir
                bitonicMerge o (low+k) k  dir
        loopSwap o low i k dir = 
            when (i < low + k) $ do
                compareAndSwap o i (i+k) dir
                loopSwap o low (i+1) k dir
        compareAndSwap o i j dir = do
            oi <- M.read o i
            oj <- M.read o j
            when (dir == (oi > oj)) $ M.swap o i j
             
mergeSeq :: Ord a => V.Vector a -> V.Vector a
mergeSeq = merge . runs

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
    a' <- V.thaw a
    b' <- V.thaw b
    go a' b' 0 0 v
    return v
        where go a' b' i j v 
                | i < V.length a && j < V.length b = do
                    ai <- M.unsafeRead a' i
                    bj <- M.unsafeRead b' j
                    if ai <= bj then do
                        M.unsafeWrite v (i+j) ai
                        go a' b' (i+1) j v
                    else do
                        M.unsafeWrite v (i+j) bj
                        go a' b' i (j+1) v
                | i < V.length a = do 
                    ai <- M.unsafeRead a' i
                    M.unsafeWrite v (i+j) ai
                    go a' b' (i+1) j v
                | j < V.length b = do 
                    bj <- M.unsafeRead b' j 
                    M.unsafeWrite v (i+j) bj
                    go a' b' i (j+1) v
                | otherwise = return ()


quickSeq :: Ord a => V.Vector a -> V.Vector a
quickSeq v = V.create $ do
    v' <- V.thaw v
    quickSort' v' 0 (V.length v - 1)
    return v'
    where
    quickSort' v low high 
        | low < high = do
            pi <- partition v low high
            quickSort' v low (pi - 1)
            quickSort' v (pi + 1) high
        | otherwise = return ()
    partition v low high = do
        i <- go (low - 1) low 
        M.swap v (i+1) high
        return $ i + 1
        where
            go i j 
                | j < high = do
                    vj <- M.read v j
                    pivot <- M.read v high
                    if vj < pivot then do
                        M.swap v (i+1) j
                        go (i+1) (j+1)
                    else
                        go i (j+1)
                | otherwise = return i 
