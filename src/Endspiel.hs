{-# LANGUAGE TupleSections, BangPatterns #-}
module Endspiel (Game (..), Status (..), move, step) where

import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set  (Set)
import qualified Data.Set as S
import Data.Foldable
import Data.Ord


------------------------------ Game typeclass -------------------------------------------
class (Ord pos, Show pos) => Game pos where
  moves    :: pos -> [pos]
  preMoves :: pos -> [pos]
  endLoses :: [pos]
  endWins  :: [pos]

  buildTable :: Int -> (Map pos Int, Map pos Int)
  buildTable maxd = helper 0 (wrap endWins) (wrap endLoses) (S.fromList endLoses)  where
    wrap = M.fromList . map (, 0)
    helper d w l p | S.null p || d == maxd    =  (w, l)
                   | otherwise = case step d w l p of (w', l', p') -> helper (d+1) w' l' p'  

step :: Game pos => Int -> Map pos Int -> Map pos Int -> Set pos -> (Map pos Int, Map pos Int, Set pos)
step d wins loses ps = let
                         undo  = S.fromList . concatMap preMoves . S.toList  
                         undo1 = S.filter (not . (`M.member` wins))  (undo ps)
                         undo2 = S.filter (not . (`M.member` loses)) (undo undo1)
                         wins' = wins `M.union` M.fromSet (const d) undo1
                         newLoses = S.filter (all (`M.member` wins') . moves) undo2
                         loses' = loses `M.union` M.fromSet (const (d+1)) newLoses
                       in 
                         (wins',loses',newLoses)
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
move mv | elem mv endWins  = (Nothing, Lose 0)
        | elem mv endLoses = (Nothing, Win 0)
        | otherwise = let
                        (loses, wins) = buildTable (-1)
                        positions = (M.map Win wins `M.union` M.map Lose loses)
                        join p = toList $ (,) p <$> M.lookup p positions
                        movesAvail  = concatMap join (moves mv)
                        best = (minimumBy (comparing snd) movesAvail)
                      in
                        (Just $ fst best, snd best) 

-------------------------------------------------------------------------------------------

