module Lib
    ( readLines
    ) where

import System.IO (withFile, IOMode(ReadMode), hIsEOF)
import qualified Data.ByteString as B
import qualified Data.Vector as V



readLines :: String -> IO (V.Vector B.ByteString)
readLines filename = withFile filename ReadMode ((V.fromList <$>) . getLines)
    where
    getLines handle = do
        eof <- hIsEOF handle  
        if eof then 
            return [] 
        else
            (:) <$> B.hGetLine handle <*> getLines handle 


