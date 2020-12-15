module Parallel (bitonicPar, mergePar, merge2) where
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Control.Monad (when)
import Control.Monad.Par (NFData, put_, runPar, new, fork, put, get )
import Control.DeepSeq (NFData)
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

mergePar :: (NFData a, Ord a) => V.Vector a -> V.Vector a
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

merge :: (NFData a, Ord a) => V.Vector (V.Vector a) -> V.Vector a
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



merge2 :: (NFData a, Ord a) => V.Vector a -> V.Vector a -> V.Vector a       
merge2 a b = 
-- merge2 a b = runPar $ do
--     fork (put x lower)
--     fork (put y upper)
--     a' <- get x
--     b' <- get y
--     return $ (V.++) a' b'
    (V.++) lower upper
    where
        n = V.length a + V.length b 
        h = n `div` 2
        third (_,_,x) = x
        lower = third <$> V.postscanl' accumLower (0,0,undefined) (V.enumFromN 0 h)
        accumLower (i, j, _) _
            | i < V.length a && j < V.length b =
                if a!i <= b!j then
                    (i+1, j, a!i) 
                else
                    (i, j+1, b!j)
            | i < V.length a = (i+1, j, a!i)
            | otherwise = (i, j+1, b!j)
        upper = V.reverse $ third <$> V.postscanl' accumUpper (V.length a - 1,V.length b - 1,undefined) (V.enumFromN 0 (n-h))
        accumUpper (i, j, _) _ 
            | i > 0 && j > 0 =
                if a!i >= b!j then
                    (i-1, j, a!i) 
                else
                    (i, j-1, b!j)
            | i > 0 = (i-1, j, a!i)
            | otherwise = (i, j-1, b!j)
