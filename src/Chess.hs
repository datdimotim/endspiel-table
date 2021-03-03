{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TupleSections, LambdaCase #-}

module Chess where
import Endspiel
import Repl
import ChessTypes
import ChessPrint

import Text.Read ()
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.ST
import Data.Monoid
import Data.Maybe (fromMaybe, listToMaybe, isNothing, isJust)
import Data.List (nub, sort, permutations)
import qualified Data.Map.Strict as M (Map, toList)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Foldable (toList, find, traverse_)
import Data.Array (Array, Ix, listArray, (!), (//), assocs)
import qualified Data.HashTable.IO as H
import Data.Hashable
import qualified Data.HashTable.IO as HT
type HashTable k v = H.BasicHashTable k v

--play357 = play $ G357 [5,5,5]

               

emptyBoard :: Board
emptyBoard = mkBoard [] White
               
placeFigure :: Field -> Coords -> Board -> Board
placeFigure Nothing c =  mapFields $ filter ((/= c) . fst)
placeFigure (Just fig) c = let
                             mapper = ((c, fig) :) . filter ((/= c) . fst)
                           in
                             mapFields mapper

getField :: Coords -> Board -> Field
getField c b = snd <$> find ((==c) . fst) (getFields b)

doMove :: Coords -> Coords -> Board -> Board
doMove c c' board = let
                      s = getMoveSide board
                      fg = getField c board
                      f'  = getFields . placeFigure Nothing c . placeFigure fg c' $ board
                    in
                      mkBoard f' (invColor s)
                  

isEmptyField :: Board -> Coords -> Bool
isEmptyField b c = isNothing (getField c b)

applyOffset :: Coords -> Offset -> Maybe Coords
applyOffset (Coords l n) (Offset dl dn) = let
                                            l' = l+dl
                                            n' = n+dn
                                            inBounds = l' >= 0 && n' >= 0 && l' <= 7 && n' <= 7
                                          in 
                                            if inBounds
                                            then Just $ Coords (l+dl) (n+dn)
                                            else Nothing





iterateOffset :: Board -> Coords -> Offset -> [Coords]
iterateOffset b c dc = case applyOffset c dc of 
                         Just c' -> if not $ isEmptyField b c'
                                    then [c']
                                    else c' : iterateOffset b c' dc
                         Nothing -> []
                                

offsetsFromRawList :: [(Int, Int)] -> [Offset]
offsetsFromRawList = map $ uncurry Offset

kingMoves :: Board -> Coords -> [Coords]
kingMoves b c = let 
                  rawList = [(1,0),(1,1),(0,1),(0-1,1),(0-1,0),(0-1,0-1),(0,0-1),(1,0-1)]
                in 
                  concatMap (toList . applyOffset c) . offsetsFromRawList $ rawList
                  
knightMoves :: Board -> Coords -> [Coords]
knightMoves b c = let 
                    rawList = [(2,1), (1,2),(1,0-2),(0-2,0-1),(0-1,0-2),(2,0-1),(0-2,0+1),(0-1,0+2)]
                  in 
                    concatMap (toList . applyOffset c) . offsetsFromRawList $ rawList

bishopMoves :: Board -> Coords -> [Coords]
bishopMoves b c = let 
                    rawList = [(1,1),(0-1,1),(0-1,0-1),(1,0-1)]
                  in 
                    concatMap (iterateOffset b c) . offsetsFromRawList $ rawList


filterSelfBeatMoves :: Color -> Board -> [Coords] -> [Coords]
filterSelfBeatMoves c b = let
                            f = toList . filterSelfBeats c b
                          in concatMap f

filterSelfBeats :: Color -> Board -> Coords -> Maybe Coords
filterSelfBeats clr b coord = case getField coord b of
                                            Nothing -> Just coord
                                            Just (Fig _ c) -> if c == clr 
                                                              then Nothing
                                                              else Just coord 

isMate :: Board -> Bool
isMate board = let
                 ms = getMoveSide board
                 mk = findFigExactOne board (Fig King ms)
               in
                 fromMaybe False $ do
                 k <- mk
                 return (isBeatField board (invColor ms) k && (null . availMoves) board)
                              

findFig :: Board -> Fig -> [(Coords, Fig)]
findFig board (Fig fig clr) = filter ((== fig) . getFigType . snd) . getColoredFigures clr $ board

findFigExactOne :: Board -> Fig -> Maybe Coords
findFigExactOne b f = case findFig b f of
                                   [(c, _)] -> Just c
                                   _ -> Nothing

posIsValid :: Board -> Bool
posIsValid board = let
                     ms = getMoveSide board
                     findKing c = findFigExactOne board (Fig King c)
                   in
                     fromMaybe False $ do
                       c <- findKing (invColor ms)
                       findKing ms
                       return $ not (isBeatField board ms c)



getColoredFigures :: Color -> Board -> [(Coords, Fig)]
getColoredFigures c = filter ((== c) . getColor . snd) . getFields

isBeatField :: Board -> Color -> Coords -> Bool
isBeatField b clr c =  let
                         fgs = getColoredFigures clr b
                         beatFields = concatMap (\(c, f) -> figureMoves f b c) fgs
                       in
                         c `elem` beatFields
                 

figureMoves :: Fig -> Board -> Coords -> [Coords]
figureMoves (Fig King clr) b c = filterSelfBeatMoves clr b $ kingMoves b c
figureMoves (Fig Knight clr) b c = filterSelfBeatMoves clr b $ knightMoves b c
figureMoves (Fig Bishop clr) b c = filterSelfBeatMoves clr b $ bishopMoves b c


availMovesBoard :: Board -> [Board]
availMovesBoard b = map (\(c, c') -> doMove c c' b) (availMoves b)

prevMovesBoard :: Board -> [Board]
prevMovesBoard board = let
                         ms = getMoveSide board
                         fgs = getColoredFigures (invColor ms) board
                       in
                         do
                           (c, fg) <- fgs
                           c' <- figureMoves fg board c
                           guard $ isEmptyField board c'
                           let b' = doMove c c' board
                           guard $ posIsValid b'
                           return b'

availMoves :: Board -> [(Coords, Coords)]
availMoves board = let
                     ms = getMoveSide board
                     fgs = getColoredFigures ms board
                   in
                     do
                       (c, fg) <- fgs
                       c' <- figureMoves fg board c
                       let b' = doMove c c' board
                       guard $ posIsValid b'
                       return (c, c')

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

instance Game Board where
  moves = availMovesBoard                               
  preMoves = prevMovesBoard
  endLoses = loses --[mateBoard]
  endWins = []


instance Hashable BoardInt where
  hashWithSalt i (BoardInt n) = hashWithSalt i n

buildInteractiveChess :: IO (Map.Map BoardInt Int, Map.Map BoardInt Int)
buildInteractiveChess = buildTableInteractive

buildInteractiveChessM :: IO (HashTable BoardInt Int, HashTable BoardInt Int)
buildInteractiveChessM = buildTableInteractiveM


{-

depth: 0  wins: 3  loses: 2  newPos: 1
depth: 1  wins: 6  loses: 5  newPos: 3
depth: 2  wins: 42  loses: 26  newPos: 21
depth: 3  wins: 306  loses: 222  newPos: 196
depth: 4  wins: 1762  loses: 524  newPos: 302
depth: 5  wins: 4616  loses: 2363  newPos: 1839
depth: 6  wins: 12888  loses: 3546  newPos: 1183
depth: 7  wins: 20156  loses: 7498  newPos: 3952
depth: 8  wins: 32178  loses: 9988  newPos: 2490
depth: 9  wins: 41511  loses: 13736  newPos: 3748
depth: 10  wins: 53351  loses: 16611  newPos: 2875
depth: 11  wins: 63949  loses: 21760  newPos: 5149
depth: 12  wins: 82470  loses: 29357  newPos: 7597
depth: 13  wins: 109524  loses: 44678  newPos: 15321
depth: 14  wins: 152652  loses: 64462  newPos: 19784
depth: 15  wins: 203544  loses: 85101  newPos: 20639
depth: 16  wins: 246205  loses: 99229  newPos: 14128
depth: 17  wins: 280750  loses: 113470  newPos: 14241
depth: 18  wins: 311576  loses: 126370  newPos: 12900
depth: 19  wins: 341170  loses: 140796  newPos: 14426
depth: 20  wins: 376783  loses: 162691  newPos: 21895
depth: 21  wins: 429901  loses: 196860  newPos: 34169
depth: 22  wins: 515517  loses: 255965  newPos: 59105
depth: 23  wins: 643056  loses: 334511  newPos: 78546
depth: 24  wins: 789230  loses: 414130  newPos: 79619
depth: 25  wins: 928570  loses: 492420  newPos: 78290
depth: 26  wins: 1067225  loses: 579854  newPos: 87434
depth: 27  wins: 1219068  loses: 682241  newPos: 102387
depth: 28  wins: 1393581  loses: 807904  newPos: 125663
depth: 29  wins: 1607204  loses: 973462  newPos: 165558
depth: 30  wins: 1866988  loses: 1196496  newPos: 223034
depth: 31  wins: 2175093  loses: 1480472  newPos: 283976
depth: 32  wins: 2491337  loses: 1801832  newPos: 321360
depth: 33  wins: 2767712  loses: 2103083  newPos: 301251
depth: 34  wins: 2987792  loses: 2351568  newPos: 248485
depth: 35  wins: 3179164  loses: 2577903  newPos: 226335
depth: 36  wins: 3369458  loses: 2817569  newPos: 239666
depth: 37  wins: 3589395  loses: 3104396  newPos: 286827
depth: 38  wins: 3885168  loses: 3478522  newPos: 374126
depth: 39  wins: 4260849  loses:
Process finished with exit code 130 (interrupted by signal 2: SIGINT)



-}

buildTableInteractive :: Game pos => IO (Map.Map pos Int, Map.Map pos Int)
buildTableInteractive = helper 0 (wrap endWins) (wrap endLoses) (Set.fromList endLoses)  where
    wrap = Map.fromList . map (, 0)
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
                            psl <- map fst <$> HT.toList ps
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
                          psT <- HT.fromList $ map (,()) ps
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

                          map fst <$> HT.toList newLoses
                          --wins' = wins `Map.union` fromSet (const d) undo1


                          --newLoses = Set.filter (all (`Map.member` wins') . moves)
                            --                                    . Set.filter (not . (`Map.member` loses))
                            --                                   $ undo2
                          --loses' = loses `Map.union` fromSet (const (d+1)) newLoses
                         --in (wins',loses',newLoses)


depth = 16
table = buildTable depth :: (M.Map Board Int, M.Map Board Int)
longestLoses =  filter ((==(depth-1)) . snd) . M.toList . fst $ table


mainFunc :: IO ()
mainFunc = void buildInteractiveChess --printBoard . fst $ (longestLoses !! 0)
--mainFunc = print (length loses)
mainFunc1 = do
            print (length loses)
            printBoard ((reverse loses) !! 0)



{-
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
-}

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
nextFields c = [c .. maxBound] 


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
          
          

loses :: [Board]
loses = do
         a <- allFields
         b <- nextFields a
         c <- nextFields b
         d <- nextFields c
         [f1, f2, f3, f4] <- permutations [Fig King White, Fig King Black, Fig Knight White, Fig Bishop White]
         let pos =   placeFigure (Just f1) a
                   . placeFigure (Just f2) b
                   . placeFigure (Just f3) c
                   . placeFigure (Just f4) d
                   . mapMoveSide (const Black)
                   $ emptyBoard

         guard $ posIsValid pos
         guard $ isMate pos --450020
         return pos


newtype BoardInt = BoardInt {getBoardInt :: Int} deriving (Show, Eq, Ord)

instance Game BoardInt where
  moves    = map (BoardInt . fromEnum) . availMovesBoard . toEnum . getBoardInt             
  preMoves = map (BoardInt . fromEnum) . prevMovesBoard . toEnum . getBoardInt
  endLoses = map (BoardInt . fromEnum) [mateBoard]--loses
  endWins  = map (BoardInt . fromEnum) ([] :: [Board])


{-

foo :: FooType a => a
foo = bar ""

class FooType a where
  bar :: String -> a
  
instance FooType String where
  bar = id
  
instance (Show x, FooType r) => FooType (x -> r) where
  bar s x = bar (s ++ show x)
  
-}


                  
