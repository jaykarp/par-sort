{-# LANGUAGE DeriveGeneric #-} 
module Utils 
   (
       fillBitonic,
       readLines,
       shuffle,
       Dumb,
       time,
       timeIO
   ) 
   where

import Data.Time.Clock ( 
    diffUTCTime, 
    getCurrentTime, 
    NominalDiffTime )
import System.IO (withFile, IOMode(ReadMode), hIsEOF)
import Data.Vector ((!))
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import System.Random ( randomRIO )
import Control.Monad (forM_)
import GHC.Generics (Generic)
import Control.DeepSeq ( NFData, force )

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

newtype Dumb = Dumb (Integer) deriving (Generic, Show)

instance Eq (Dumb) where
    (Dumb 0) == (Dumb 0) = True
    (Dumb x) == (Dumb 0) = False
    (Dumb 0) == (Dumb y) = False 
    (Dumb x) == (Dumb y) = Dumb (x-1) == Dumb (y-1)

instance Ord (Dumb) where
    (Dumb x) <= (Dumb 0) = x <= 0 
    (Dumb 0) <= (Dumb y) = y >= 0 
    (Dumb x) <= (Dumb y) = (Dumb (x-1)) <= (Dumb (y-1)) 

instance Num (Dumb) where
    (+) (Dumb a) (Dumb b) = Dumb $ a + b
    (*) (Dumb a) (Dumb b) = Dumb $ a * b
    abs (Dumb a) = Dumb $ abs a
    fromInteger = Dumb
    negate (Dumb a) = Dumb $ -a
    signum (Dumb a) = Dumb $ signum a

instance NFData (Dumb)

time :: (NFData a) => String -> a -> IO () 
time msg a = do
    start <- getCurrentTime
    let a' = force a
    end <- a' `seq` getCurrentTime
    putStrLn $ show (diffUTCTime end start)

timeIO :: NFData a => [Char] -> IO a -> IO ()
timeIO msg io = do
    start <- getCurrentTime
    a <- io
    end <- (force a) `seq` getCurrentTime
    putStrLn $ show (diffUTCTime end start)
