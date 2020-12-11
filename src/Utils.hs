module Utils 
   (
       fillBitonic,
       readLines,
       shuffle
   ) 
   where

import System.IO (withFile, IOMode(ReadMode), hIsEOF)
import Data.Vector ((!))
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import System.Random ( randomRIO )
import Control.Monad (forM_)

shuffle :: V.Vector a -> IO (V.Vector a)
shuffle v = do
   let n = V.length v - 1
   js <- V.forM (V.enumFromTo 0 n) $ \i -> randomRIO (i, n)
   return $ V.create $ do
      o <- V.thaw v 
      forM_ [1..n] $ \i -> M.swap o i (js!i)
      return o


fillBitonic :: a -> V.Vector a -> V.Vector a
fillBitonic a v = V.create $ do
    o <- V.thaw v
    let l = V.length v
    let n = 2 ^ ceiling (logBase 2 (fromIntegral l)) - l
    o' <- M.grow o n 
    p <- M.replicate n a
    M.copy (M.slice l n o') p
    return o'

readLines :: String -> IO (V.Vector B.ByteString)
readLines filename = withFile filename ReadMode ((V.fromList <$>) . getLines)
    where
    getLines handle = do
        eof <- hIsEOF handle  
        if eof then 
            return [] 
        else
            (:) <$> B.hGetLine handle <*> getLines handle 
