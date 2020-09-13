{-# LANGUAGE TupleSections #-}

module Endspiel (Game (..), Status (..), move) where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (filter, map, null)
import qualified Data.Map.Strict as Map
import Data.Set hiding (map)
import qualified Data.Set as Set
import qualified Data.Foldable as F
import Data.Ord


------------------------------ Game typeclass -------------------------------------------
class (Ord pos, Show pos) => Game pos where
  moves    :: pos -> [pos]
  preMoves :: pos -> [pos]
  endLoses :: [pos]
  endWins  :: [pos]

  buildTable :: (Map pos Int, Map pos Int)
  buildTable = helper 0 (wrap endWins) (wrap endLoses) (Set.fromList endLoses)  where
    wrap = Map.fromList . map (, 0)
    helper d w l p | Set.null p    =  (w, l)
                   | otherwise = case step d w l p of (w', l', p') -> helper (d+1) w' l' p'  

step :: Game pos => Int -> Map pos Int -> Map pos Int -> Set pos -> (Map pos Int, Map pos Int, Set pos)
step d wins loses ps = let
                          undo  = Set.fromList . concatMap preMoves . Set.toList  
                          undo1 = undo ps 
                          undo2 = undo undo1
                          wins' = wins `Map.union` fromSet (const d) undo1
                          newLoses = Set.filter (all (`Map.member` wins') . moves) undo2
                          loses' = loses `Map.union` fromSet (const (d+1)) newLoses
                         in (wins',loses',newLoses)
------------------------------------------------------------------------------------------

------------------------------ Move engine -----------------------------------------------
data Status = Win Int | Lose Int | Draw deriving (Show, Eq)

instance Ord Status where
  Win _  <= Draw   = True
  Win _  <= Lose _ = True
  Draw   <= Lose _ = True
  Draw   <= Win  _ = False
  Lose _ <= Win  _ = False
  Lose _ <= Draw   = False
  Win a  <= Win b  = a <= b
  Lose a <= Lose b = b <= a

    

move :: Game pos => pos -> (Maybe pos, Status)
move mv | mv `elem` endWins  = (Nothing, Lose 0)
        | mv `elem` endLoses = (Nothing, Win 0)
        | otherwise = let
                        (loses, wins) = buildTable
                        positions = (Map.map Win wins `Map.union` Map.map Lose loses)
                        join p = F.toList $ (,) p <$> lookup p positions
                        movesAvail  = concatMap join (moves mv)
                        best = F.minimumBy (comparing snd) movesAvail
                      in
                        (Just $ fst best, snd best) 

-------------------------------------------------------------------------------------------

