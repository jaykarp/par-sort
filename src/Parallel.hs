module Parallel (bitonicPar, mergePar) where
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
          let b' = (force (merge' (l+1) b))
          rseq a'
          if l < 1 then merge2Par a' b' else return $ merge2 a' b' 
        | n > 1 = merge2 (merge' (l+1) a) (merge' (l+1) b)
        | otherwise = v!0
        where 
        n = V.length v
        a = V.slice 0 (n `div` 2) v
        b = V.slice (n `div` 2) (n - n `div` 2) v

-- merge :: (NFData a, Ord a) => V.Vector (V.Vector a) -> IO (V.Vector a)
-- merge v = do
--     o <- V.thaw v 
--     mergeAll (V.length v) o
--     M.read o 0 
--     where
--         mergeAll k o 
--             | k == 1 = return ()  
--             | otherwise = do
--                 k' <- mergePairs 0 k o 
--                 mergeAll k' o
--         mergePairs i k o
--             | i < k - 1 = do
--                 oi <- M.read o i 
--                 oip1 <- M.read o (i+1)
--                 if k > 800 then do
--                     M.write o (i `div` 2) (merge2 oi oip1)
--                     mergePairs (i+2) k o
--                 else 
--                 runParIO $ do
--                     a <- spawn $ liftIO $ M.write o (i `div` 2) (merge2 oi oip1)
--                     b <- spawn $ liftIO $ mergePairs (i+2) k o
--                     get a
--                     get b
--             | i == k - 1 = do
--                 oi <- M.read o i
--                 M.write o (i `div` 2) oi
--                 return $ k `div` 2 + 1 
--             | otherwise = return $ k `div` 2

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
