module Parallel (bitonicPar, mergePar, merge2) where
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Control.Monad (when)
import Control.Monad.Par (put_, runPar, new, fork, put, get )
import Utils ( fillBitonic )
import Control.Monad.ST (ST)
import Debug.Trace
bitonicPar :: Ord a => a -> V.Vector a -> V.Vector a
bitonicPar = (bitonic .) . fillBitonic

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

mergePar :: Ord a => V.Vector a -> V.Vector a
mergePar = merge . runs

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
merge2 a b = V.force $ V.create $ do
    v <- M.new (V.length a + V.length b)
    a' <- V.thaw a  
    b' <- V.thaw b
    compareExchange v a' b'
    return v 
    where
        compareExchange v a' b' = runPar $ do
            x <- new
            y <- new
            fork (put_ x $ mergeUpper v a' b')
            fork (put_ y $ mergeLower v a' b')
            x' <- get x
            y' <- get y
            return (x' >> y')
        mergeUpper :: Ord a => M.MVector s a -> M.MVector s a -> M.MVector s a -> ST s ()
        mergeUpper v a' b' = do
            upper a' b' (M.length a' - 1) (M.length b' - 1) v
        mergeLower :: Ord a => M.MVector s a -> M.MVector s a -> M.MVector s a -> ST s ()
        mergeLower v a' b' = do
            lower a' b' 0 0 v 
        lower a' b' i j v 
            | i < M.length a' && j < M.length b' = when (i + j < M.length v) $ do
                ai <- M.read a' i
                bj <- M.read b' j
                if ai <= bj then do
                    M.write v (i+j) ai
                    lower a' b' (i+1) j v
                else do
                    M.write v (i+j) bj
                    lower a' b' i (j+1) v
            | i < M.length a' = when (i + j < M.length v) $ do 
                ai <- M.read a' i
                M.write v (i+j) ai
                lower a' b' (i+1) j v
            | j < M.length b' = when (i + j < M.length v) $ do 
                bj <- M.read b' j 
                M.write v (i+j) bj
                lower a' b' i (j+1) v
            | otherwise = return ()
        upper a' b' i j v  
            | i >= 0 && j >= 0 = when ((M.length a' - 1 - i) + (M.length b' - 1 - j) < M.length v) $ do
                ai <- M.read a' i
                bj <- M.read b' j
                if ai >= bj then do
                    M.write v (i + j + 1) ai
                    upper a' b' (i-1) j v
                else do
                    M.write v (i + j + 1) bj
                    upper a' b' i (j-1) v
            | i >= 0 = when ((M.length a' - 1) + (M.length b' - 1 - j) < M.length v) $ do 
                ai <- M.read a' i
                M.write v (i + j + 1) ai
                upper a' b' (i-1) j v
            | j >= 0 = when ((M.length a' - 1) + (M.length b' - 1 - j) < M.length v) $ do 
                bj <- M.read b' j 
                M.write v (i + j + 1) bj
                upper a' b' i (j-1) v
            | otherwise = return ()