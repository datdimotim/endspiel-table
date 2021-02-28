module Main where

import Game357
import Chess
import System.IO

main :: IO ()
main = do
         hSetBuffering stdout NoBuffering
         mainFunc
