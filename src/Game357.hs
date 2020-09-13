{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Game357 where
import Endspiel
import Repl

import Text.Read hiding (step)
import Control.Monad

play357 = play $ G357 [5,5,5]




numColumns = 3
maxCapacity = 5
newtype G357 = G357 [Int] deriving (Eq, Ord, Show, Read) 

movesH :: (Int -> [Int]) -> (G357 -> [G357])
movesH f (G357 as) = map G357 . helper $ as where
    helper [a]    = map return (f a)
    helper (a:as) = let
                      fs = map (:as) (f a)
                      sn = map (a:) (helper as)
                    in fs ++ sn

instance Game G357 where
  moves = movesH $ \a -> [0 .. a-1]                                 
  preMoves = movesH $ \a -> [a+1 .. maxCapacity]
  endLoses = map G357 [[1,0,0], [0,1,0], [0,0,1]]
  endWins = [G357 [0,0,0]]


instance MoveReader G357 (Int,Int) where
  applyMove (G357 s) a = let
                    setAt i e l = take i l ++ [e] ++ drop (i+1) l 
                  in 
                    do 
                      (col, cnt) <- return a 
                      guard $ col >= 1 && col <= 3
                      let r = (s !! (col-1)) - cnt
                      let s' = setAt (col-1) r s
                      return $ G357 s' 


                  
