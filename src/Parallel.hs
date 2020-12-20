module Parallel (bitonicPar, mergePar, merge) where
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
-- import Control.Monad.Par (NFData, put_, runPar, new, fork, put, get )
import Control.Monad.Par.Class
import Control.Monad.Par.IO (runParIO) 
import System.IO.Unsafe (unsafePerformIO)
import Control.Parallel (par)
import Control.Parallel.Strategies 
import Control.DeepSeq (NFData, force)
import Utils ( fillBitonic )
import Debug.Trace

bitonicPar :: Ord a => a -> V.Vector a -> IO (V.Vector a)
bitonicPar = (bitonic .) . fillBitonic

bitonic :: Ord a => V.Vector a -> IO (V.Vector a)
bitonic v = do
    o <- V.thaw v 
    bitonicSort' o 0 (V.length v) True 
    V.freeze o
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
                bitonicMerge o (low+k) k dir
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



merge :: (NFData a, Ord a) => V.Vector (V.Vector a) -> (V.Vector a)
merge v = merge' 0 v
    where
    merge' l v
        
        | n > 1 = runEval $ do
          a' <- rpar (force (merge' (l+1) a))
          b' <- (force (merge' (l+1) b))
          rseq a'
          rseq b'
          if l < 1 then merge2Par a' b' else return $ merge2 a' b' 
        | n > 1 = merge2 (merge' (l+1) a) (merge' (l+1) b)
        | otherwise = v!0
        where 
        n = V.length v
        a = V.slice 0 (n `div` 2) v
        b = V.slice (n `div` 2) (n - n `div` 2) v

-- merge :: (NFData a, Ord a) => V.Vector (V.Vector a) -> (V.Vector a)
-- merge v = runEval (merge' 0 v)
--     where
--     -- merge' :: Int -> V.Vector (V.Vector a) -> Eval (V.Vector a)
--     merge' l v
--        | n > 1 = do
--             a' <- (merge' (l+1) a) >>= rpar
--             b' <- (merge' (l+1) b) >>= if l > 2 then rpar else rseq
--             if l < 1 then merge2Par a' b' else return $ merge2 a' b'
--         | otherwise = return $ v!0
--         where 
--         n = V.length v
--         a = V.slice 0 (n `div` 2) v
--         b = V.slice (n `div` 2) (n - n `div` 2) v
-- 
-- merge :: (NFData a, Ord a) => V.Vector (V.Vector a) -> (V.Vector a)
-- merge v = mergeAll v 
--     where
--         mergeAll x 
--             | V.length x == 1 = x!0
--             | otherwise = mergeAll (mergePairs x)
--         mergePairs x  
--             | V.length x `mod` 2 == 0 = V.zipWith merge2 (evens x) (odds x) `using` parTraversable rseq
--             | otherwise = V.zipWith merge2 (evens x) (odds x) `V.snoc` (V.last x) `using` parTraversable rseq
--         odds = V.ifilter (\i a -> i `mod` 2 == 0)

merge2Par :: (NFData a, Ord a) => V.Vector a -> V.Vector a -> Eval (V.Vector a)       
merge2Par a b = do
    l <- rpar (force lower)
    u <- rpar (force upper) 
    return (l V.++ u)
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