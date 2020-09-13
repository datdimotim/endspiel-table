{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Chess where
import Endspiel
import Repl

import Text.Read hiding (step)
import Control.Monad

play357 = play $ G357 [5,5,5]


data FigType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Read)
data Color = White | Black deriving (Eq, Ord, Show, Read)
data Fig = Fig FigType Color deriving (Eq, Ord, Show, Read)
type Field = Maybe Fig
data Coords = Coords { vert :: Int
                     , hor  :: Int
                     } deriving (Eq, Ord, Show, Read)

data Pos = Pos { fields   :: [[Field]]
               , moveSide :: Color
               } deriving (Eq, Ord, Show, Read)

inBounds :: Coords -> Bool
inBounds (Coords v h) = v >=0 && h >=0 && v<= 7 && h<= 7

movesFromOffsets :: [(Int, Int)] -> (Coords -> [Coords])
movesFromOffsets ds (Coords v h) = let
                                     mapper (dv, dh) = Coords (v+dv) (h+dh)
                                     dsFiltered = filter (/= (0, 0)) ds
                                   in 
                                     filter inBounds $ map mapper dsFiltered

rotSyms :: (Int, Int) -> [(Int, Int)]
rotSyms (v, h) = [(v, h), (0-v, 0-h), (0-h, v), (h, 0-v)]

figureMoves :: Fig -> Coords -> [Coords]
figureMoves (Fig Queen _) = movesFromOffsets $ rotSyms (1,1) ++ rotSyms (0,1) 
figureMoves (Fig Knight _) = movesFromOffsets $ rotSyms (2,1) ++ rotSyms (1,2)
figureMoves (Fig Bishop _) = movesFromOffsets $ concatMap rotSyms [(i,i) | i <- [1..7]] 

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


                  
