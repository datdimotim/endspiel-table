{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
module Repl where

import Endspiel
import Control.Monad
import Text.Read


repl :: (Show s, Show b, MoveReader s a) => s -> (s -> (Maybe s, b)) -> IO ()
repl s f = let
               loopRead s = do
                            a <- readMove s <$> getLine
                            case a of
                              Just r -> return r
                              Nothing -> print "input error, try again" >> loopRead s
                             
               helper s = do
                        s' <- loopRead s
                        print s'
                        let (ms, b) = f s'
                        print (ms, b)
                        case ms of 
                             Just s' -> helper s'
                             Nothing -> return ()
           in helper s
           
play :: (Game p, MoveReader p a) => p -> IO ()
play p = do
           print p
           repl p move


class (Game p, Read a) => MoveReader p a | p -> a where
 applyMove :: p -> a -> Maybe p
 readMove :: p -> String -> Maybe p
 readMove p s = do
                  a  <- readMaybe s
                  p' <- applyMove p a
                  guard $ elem p' (moves p)
                  return p'
