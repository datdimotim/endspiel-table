{-# LANGUAGE TupleSections, BangPatterns #-}
module Endspiel (Game (..), Status (..), move, step, stepInt) where

import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Set  (Set)
import qualified Data.Set as S
import Data.IntSet  (IntSet)
import qualified Data.IntSet as IS
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

buildTableInt :: Int -> [Int] -> [Int] -> (Int -> [Int]) -> (Int -> [Int]) -> (IntMap Int, IntMap Int)
buildTableInt maxd endWins endLoses moves preMoves = helper 0 (wrap endWins) (wrap endLoses) (wrap endLoses)  where
    wrap = IM.fromList . map (, 0)
    helper d w l p | IM.null p || d == maxd    =  (w, l)
                   | otherwise = case stepInt moves preMoves d w l p of (w', l', p') -> helper (d+1) w' l' p'
                   
                   
                   
                         
stepInt :: (Int -> [Int]) -> (Int -> [Int]) -> Int -> IntMap Int -> IntMap Int -> IntMap Int -> (IntMap Int, IntMap Int, IntMap Int)
stepInt moves preMoves d wins loses ps = let
                         undo dp = IM.fromList . concatMap (map (, dp) . preMoves . fst) . IM.assocs
                         undo1 = IM.filterWithKey (\k v -> not (k `IM.member` wins))  (undo d ps)
                         undo2 = IM.filterWithKey (\k v -> not (k `IM.member` loses)) (undo (d+1) undo1)
                         wins' = wins `IM.union`  undo1
                         newLoses = IM.filterWithKey (\k v -> all (`IM.member` wins') (moves k)) undo2
                         loses' = loses `IM.union` newLoses
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

