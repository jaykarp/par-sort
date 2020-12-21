module Parallel (bitonicPar, mergePar, quickNaivePar, quickPar) where
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Split as S
import Control.Monad (when)
import Control.Parallel (par)
import Control.Parallel.Strategies 
import Control.DeepSeq (NFData, force)
import Utils ( fillBitonic )
import Debug.Trace
import Sequential (quickSeq)

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

-- naive quicksort algorithm
quickNaivePar :: (NFData a, Ord a) => V.Vector a -> V.Vector a
quickNaivePar v = merge $ V.fromList $ parMap rdeepseq quickSeq chunks
    where 
    n = V.length v
    chunks = S.chunksOf (n `div` 32) v

quickPar :: (NFData a, Ord a, Show a) => V.Vector a -> V.Vector a
quickPar v = runEval $ quickPar' chunks    
    where
    quickPar' :: (Show a, NFData a, Ord a) => V.Vector (V.Vector a) -> Eval (V.Vector a)
    quickPar' vs
        | V.length vs == 0 = return V.empty 
        | V.length vs == 1 = return $ quickSeq $ vs!0 
        | otherwise = do 
            let p = V.last . V.last $ vs 
            let vs' = V.init vs `V.snoc` (V.last $ V.init $ vs)
            vs'' <- parTraversable rdeepseq $ V.map (V.partition (<p)) vs'
            let lower = fst <$> vs'' 
            let upper = snd <$> vs''
            let lower' = V.fromList $ filter (not . null) $ (V.concat . V.toList) <$> S.chunksOf 2 lower  
            let upper' = V.fromList $ filter (not . null) $ (V.concat . V.toList) <$> S.chunksOf 2 upper  
            lower'' <- quickPar' lower' 
            upper'' <- quickPar' upper'
            return $ (lower'') V.++ upper''
    n = V.length v
    chunks = V.fromList $ S.chunksOf (n `div` 8) v

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
merge v = runEval (merge' 0 v)
    where
    merge' l v
       | n > 1 = do
            a' <- (merge' (l+1) a) >>= if l < 15 then rpar else rseq
            b' <- (merge' (l+1) b) >>= if l < 15 then rpar else rseq
            if l < 1 then merge2Par a' b' else return $ merge2 a' b'
        | otherwise = return $ v!0
        where 
        n = V.length v
        a = V.slice 0 (n `div` 2) v
        b = V.slice (n `div` 2) (n - n `div` 2) v

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
