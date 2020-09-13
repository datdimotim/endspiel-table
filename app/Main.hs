module Main where

import Game357
import System.IO

main :: IO ()
main = do
         hSetBuffering stdout NoBuffering
         play357
