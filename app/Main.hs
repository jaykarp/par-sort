{-# LANGUAGE DeriveGeneric, TypeOperators #-} 
module Main where

import Lib (
    readLines,
    quickSeq,
    mergeSeq,
    bitonicSeq,
    shuffle, 
    mergePar,
    bitonicPar,
    hybridPar,
    quickPar,
    time,
    timeIO )
import Data.List (sort)
import qualified Data.Vector as V
import Data.String ( fromString )
import System.Environment ( getArgs )
import System.Console.GetOpt 
import System.Exit ( die )
import Control.DeepSeq ( NFData )
-- TODO:
-- parallel or not "-p"
-- Specify which sort we want to run "-s { default, bitonic, quick, merge, hybrid }" (by default this is default (haskell sort))
    -- Specify bitonic filler "-f "
-- Specify wether we want to time it or not
-- Output or not
-- ints vs file pointer
-- whether or not to use dumb data structure and where to start it from


data Arg
    = Sort String                   -- -s
    | Input String                  -- -i
    | Size String                   -- -z 
    | Help                          -- --help
    deriving (Eq, Ord, Show)

options :: [OptDescr Arg]
options = [
      Option ['s'] ["sort"] (ReqArg Sort "") "default | bitonicSeq | mergeSeq | quickSeq | bitonicPar | mergePar | quickPar | hybrid"  
    , Option ['i'] ["input"] (ReqArg Input "") "File path"
    , Option ['z'] ["size"] (ReqArg Size "") "2^z size of array to be sorted"
    , Option ['h'] ["help"] (NoArg Help) "Print this help message"
    ]


runFromArgs :: [String] -> IO ()
runFromArgs args = case opt of  
    Help:_ -> die usage 
    (Sort s):(Input f):_ -> do
        v <- readLines f 
        if s == "bitonicSeq" || s == "bitonicPar" then
            runBitonic s (fromString "") v
        else
            runSort s v
    (Sort s):(Size z):_ -> do
        let n = read z :: Int
        v <- shuffle $ V.enumFromN (1 :: Int) (2^n)
        if s == "bitonicSeq" || s == "bitonicPar" then
            runBitonic s 0 v
        else
            runSort s v
    _ -> die usage 
    where 
    (opt,_,_) = getOpt Permute options args 

usage :: String
usage = "Usage: parsort -s [default | bitonicSeq | mergeSeq | quickSeq | bitonicPar | mergePar | quickPar | hybrid] -i [file] -z [size]"

runSort :: (NFData a, Ord a) => String -> V.Vector a -> IO ()
runSort "default"    v = time "Default Sort" (sort $ V.toList $ v)
runSort "quickSeq"   v = time "Sequential Quicksort" (quickSeq v)
runSort "mergeSeq"   v = time "Sequential Merge Sort" (mergeSeq v)
runSort "hybrid"     v = time "Parallel Hybrid Sort" (hybridPar v)
runSort "quickPar"   v = time "Parallel Quick Sort" (quickPar v)
runSort "mergePar"   v = time "Parallel Merge Sort" (mergePar v)
runSort _ _ = die usage

runBitonic :: (NFData a, Ord a) => String -> a -> V.Vector a -> IO ()
runBitonic "bitonicSeq" a v = time "Sequential Bitonic Sort" (bitonicSeq a v)
runBitonic "bitonicPar" a v = timeIO "Parallel Bitonic Sort" (bitonicPar a v)
runBitonic _ _ _ = die usage

main :: IO ()
main = getArgs >>= runFromArgs 