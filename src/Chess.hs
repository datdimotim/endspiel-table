{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TupleSections, LambdaCase, BangPatterns #-}

module Chess where
import Endspiel
import Repl
import ChessTypes
import ChessPrint
import ChessMoves

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.ST

import Data.Monoid
import Data.Maybe (fromMaybe, listToMaybe, isNothing, isJust)
import Data.List (nub, sort, permutations)
import Data.Foldable (toList, find, traverse_)

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import qualified Data.Set as S

import Data.Array (Array, Ix, listArray, (!), (//), assocs)

import qualified Data.HashTable.IO as H
import Data.Hashable
type HashTable k v = H.BasicHashTable k v


viewAvailMovesFrom :: Board -> Coords -> IO ()
viewAvailMovesFrom b c = let
                           marks = map snd . filter ((== c) . fst) . availMoves $ b
                         in 
                           viewMarks b marks (Fig Pawn White)
                                    

tstBoard =   placeFigure (Just (Fig King White)) (Coords 3 3) 
           . placeFigure (Just (Fig King Black)) (Coords 4 5) 
           . placeFigure (Just (Fig Bishop White)) (Coords 5 1) 
           . placeFigure (Just (Fig Knight White)) (Coords 6 2) 
           $ emptyBoard

instance Game Board where
  moves = availMovesBoard                               
  preMoves = prevMovesBoard
  endLoses = loses --[mateBoard]
  endWins = []


instance Hashable BoardInt where
  hashWithSalt i (BoardInt n) = hashWithSalt i n

buildInteractiveChess :: IO (Map BoardInt Int, Map BoardInt Int)
buildInteractiveChess = buildTableInteractive

buildInteractiveChessM :: IO (HashTable BoardInt Int, HashTable BoardInt Int)
buildInteractiveChessM = buildTableInteractiveM

buildTableInteractive :: Game pos => IO (Map pos Int, Map pos Int)
buildTableInteractive = helper 0 (wrap endWins) (wrap endLoses) (S.fromList endLoses)  where
    wrap = M.fromList . map (, 0)
    helper d w l p = if null p || d == 15
                     then return (w, l)
                     else
                       do
                         let (w', l', p') = step d w l p
                         putStrLn $ "depth: " ++ show d ++ "  wins: " ++ show (length w') ++ "  loses: " ++ show (length l')  ++ "  newPos: " ++ show (length p')
                         helper (d+1) w' l' p'



buildTableInteractiveM :: (Game pos, Hashable pos) => IO (HashTable pos Int, HashTable pos Int)
buildTableInteractiveM = let
                          helper d w l p = if null p
                                           then return (w, l)
                                           else
                                             do
                                               p' <- stepM d w l p
                                               w' <- H.toList w
                                               l' <- H.toList l
                                               putStrLn $ "depth: " ++ show d ++ "  wins: " ++ show (length w') ++ "  loses: " ++ show (length l')  ++ "  newPos: " ++ show (length p')
                                               helper (d+1) w l p'
                        in
                          do
                            wins <- H.new
                            traverse_ (\w -> H.insert wins w 0)  endWins
                            loses <- H.new
                            traverse_ (\w -> H.insert loses w 0)  endLoses
                            ps <- (H.new :: IO (HashTable pos ()))
                            traverse_ (\w -> H.insert ps w ())  endLoses
                            psl <- map fst <$> H.toList ps
                            helper 0 wins loses psl
                            return (wins, loses)

undo :: (Game pos, Hashable pos) => HashTable pos () -> IO (HashTable pos ())
undo setPos = do
                newSet <- H.new
                H.mapM_ (\(p, _) -> forM_ (preMoves p) (\p' -> H.insert newSet p' ())) setPos
                return newSet



addIfEmpty :: (Hashable k, Eq k) => HashTable k v -> k -> v -> IO ()
addIfEmpty ht k v = H.mutate ht k $ \case
                                    Nothing -> (Just v, ())
                                    Just old -> (Just old, ())

isPresent :: (Hashable k, Eq k) => HashTable k v -> k -> IO Bool
isPresent ht k = isJust <$> H.lookup ht k


stepM :: (Game pos, Hashable pos) => Int -> HashTable pos Int -> HashTable pos Int -> [pos] -> IO [pos]
stepM d wins loses ps = do
                          psT <- H.fromList $ map (,()) ps
                          undo1 <- undo psT
                          undo2 <- undo undo1
                          flip H.mapM_ undo1 $ \(k, _) -> do {
                               addIfEmpty wins k d
                          }

                          newLoses <- (H.new :: IO (HashTable pos ()))
                          flip H.mapM_ undo2 $ \(k, v) -> runMaybeT $ do {
                               present <- lift $ isJust <$> H.lookup loses k;
                               guard $ not present;
                               isAllWins <- lift $ and <$> traverse (isPresent wins) (moves k);
                               guard isAllWins;
                               lift $ H.insert newLoses k ();
                          }

                          flip H.mapM_ newLoses $ \(k, _) -> do {
                               addIfEmpty loses k (d+1)
                          }

                          map fst <$> H.toList newLoses
                   


depth = 16
table = buildTable depth :: (M.Map Board Int, M.Map Board Int)
longestLoses =  filter ((==(depth-1)) . snd) . M.toList . fst $ table


mainFunc :: IO ()
mainFunc = void buildInteractiveChess --printBoard . fst $ (longestLoses !! 0)

--mainFunc = print . map (length . snd) . IM.assocs $ availMovesM  

--mainFunc = print (length loses)
mainFunc11 = do
            print (length loses)
            printBoard ((reverse loses) !! 0)
            
mainFunc2 = let
             st = BoardInt $ fromEnum mateBoard
             ps = return st >>= preMoves >>= preMoves >>= preMoves >>= preMoves >>= preMoves >>= preMoves >>= preMoves >>= preMoves >>= preMoves >>= preMoves
           in
             print $ length (ps :: [BoardInt])

viewMarks :: Board -> [Coords] -> Fig -> IO ()
viewMarks b cs f = let
                     fillBoard = appEndo . foldMap (Endo . placeFigure (Just f)) $ cs
                   in
                     printBoard $ fillBoard b



viewMoves :: Board -> Coords -> Fig -> IO ()
viewMoves b c f = let
                    cs = figureMoves f b c
                  in
                    viewMarks b cs f
                    

allFields :: [Coords]
allFields = [minBound .. maxBound]

nextFields :: Coords -> [Coords]            
nextFields c = [succ c .. maxBound] 


mateBoard = placeFigure (Just (Fig King Black)) (Coords 0 7)
          . placeFigure (Just (Fig King White)) (Coords 1 5)
          . placeFigure (Just (Fig Knight White)) (Coords 0 5)
          . placeFigure (Just (Fig Bishop White)) (Coords 6 1)
          . mapMoveSide (const Black)
          $ emptyBoard
          
          
lastBoard = placeFigure (Just (Fig King Black)) (Coords 7 4)
          . placeFigure (Just (Fig King White)) (Coords 7 5)
          . placeFigure (Just (Fig Knight White)) (Coords 7 6)
          . placeFigure (Just (Fig Bishop White)) (Coords 7 7)
          . mapMoveSide (const Black)
          $ emptyBoard
          
allPositions :: [Board]
allPositions = do
         a <- allFields
         b <- nextFields a
         c <- nextFields b
         d <- nextFields c
         [f1, f2, f3, f4] <- permutations [Fig King White, Fig King Black, Fig Knight White, Fig Bishop White]
         moveSide <- [Black, White]
         let pos =   placeFigure (Just f1) a
                   . placeFigure (Just f2) b
                   . placeFigure (Just f3) c
                   . placeFigure (Just f4) d
                   . mapMoveSide (const moveSide)
                   $ emptyBoard

         guard $ posIsValid pos
         return pos

loses :: [Board]
loses = filter (\b -> getMoveSide b == Black && isMate b) allPositions

availMovesM :: IntMap [BoardInt]
availMovesM = IM.fromList $ map (\b -> (fromEnum b, map (BoardInt . fromEnum) (availMovesBoard b))) allPositions


newtype BoardInt = BoardInt {getBoardInt :: Int} deriving (Show, Eq, Ord)

instance Game BoardInt where
  moves    = map (BoardInt . fromEnum) . availMovesBoard . toEnum . getBoardInt             
  preMoves = map (BoardInt . fromEnum) . prevMovesBoard . toEnum . getBoardInt
  endLoses = map (BoardInt . fromEnum)  [mateBoard]--loses
  endWins  = map (BoardInt . fromEnum) ([] :: [Board])


                  
