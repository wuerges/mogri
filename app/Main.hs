module Main where

import Lib
import System.Environment

main :: IO ()
main = do
     [input, output, size] <- getArgs
     shrinkToMaxSize input output (read size)
