{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TupleSections #-}

module Chess where
import Endspiel
import Repl
import ChessTypes
import ChessPrint

import Text.Read hiding (step)
import Control.Monad
import Data.Monoid
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (nub)
import qualified Data.Map.Strict as M (Map, toList)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)
import Data.Array (Array, Ix, listArray, (!), (//), assocs)

--play357 = play $ G357 [5,5,5]

               

emptyBoard :: Board
emptyBoard = let
               f = listArray ((Coords 0 0), (Coords 7 7)) (take 64 $ repeat Nothing)
             in 
               Board f White            
               
placeFigure :: Field -> Coords -> Board -> Board
placeFigure fig c (Board f clr) = let
                                    f' = f // [(c, fig)]
                                  in 
                                    Board f' clr    
                                    

doMove :: Coords -> Coords -> Board -> Board
doMove c c' (Board f s) = let
                            fg = f ! c
                          in
                            Board (f // [(c, Nothing), (c', fg)]) (invColor s)
                  

isEmptyField :: Board -> Coords -> Bool
isEmptyField (Board f _ ) c = f ! c == Nothing

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
                         Just c' -> if (not $ isEmptyField b c')
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
filterSelfBeats clr (Board field _) coord = case field ! coord of
                                            Nothing -> Just coord
                                            Just (Fig _ c) -> if c == clr 
                                                              then Nothing
                                                              else Just coord 

isMate :: Board -> Bool
isMate board@(Board f ms) = let
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
                                   otherwise -> Nothing

posIsValid :: Board -> Bool
posIsValid board@(Board f ms) = let
                                  findKing c = findFigExactOne board (Fig King c)         
                                in
                                  fromMaybe False $ do
                                    c <- findKing (invColor ms)
                                    findKing ms
                                    return $ not (isBeatField board ms c)
                 

getFigures :: Board -> [(Coords, Fig)]
getFigures = concatMap (\(c, m) -> toList . fmap (c,) $ m) . assocs . getFields


getColoredFigures :: Color -> Board -> [(Coords, Fig)]
getColoredFigures c = filter ((== c) . getColor . snd) . getFigures

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
prevMovesBoard board@(Board _ ms) = let
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
availMoves board@(Board f ms) = let
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
data G357 = G357 [Int] deriving (Eq, Ord, Show, Read) 

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
  endLoses = [mateBoard]
  endWins = []



buildInteractiveChess :: IO (Map.Map BoardInt Int, Map.Map BoardInt Int)
buildInteractiveChess = buildTableInteractive


{-

depth: 0  wins: 3  loses: 2  newPos: 1
depth: 1  wins: 6  loses: 5  newPos: 3
depth: 2  wins: 42  loses: 26  newPos: 22
depth: 3  wins: 306  loses: 222  newPos: 199
depth: 4  wins: 1762  loses: 524  newPos: 329
depth: 5  wins: 4616  loses: 2363  newPos: 2086
depth: 6  wins: 12888  loses: 3546  newPos: 1894
depth: 7  wins: 20156  loses: 7498  newPos: 6897
depth: 8  wins: 32178  loses: 9988  newPos: 7904
depth: 9  wins: 41511  loses: 13736  newPos: 13606
depth: 10  wins: 53351  loses: 16611  newPos: 16208
depth: 11  wins: 63949  loses: 21760  newPos: 21737
depth: 12  wins: 82470  loses: 29357  newPos: 29326
depth: 13  wins: 109524  loses: 44678  newPos: 44677
depth: 14  wins: 152652  loses: 64462  newPos: 64461
depth: 15  wins: 203544  loses: 85101  newPos: 85100
depth: 16  wins: 246205  loses: 99229  newPos: 99228
depth: 17  wins: 280750  loses: 113470  newPos: 113469
depth: 18  wins: 311576  loses: 126370  newPos: 126369
depth: 19  wins: 341170  loses: 140796  newPos: 140795
depth: 20  wins: 376783  loses: 162691  newPos: 162690
depth: 21  wins: 429901  loses: 196860  newPos: 196859
depth: 22  wins: 515517  loses: 255965  newPos: 255964
depth: 23  wins: 643056  loses: 334511  newPos: 334510
depth: 24  wins: 789230  loses: 414130  newPos: 414129
depth: 25  wins: 928570  loses: 492420  newPos: 492419
depth: 26  wins: 1067225  loses: 579854  newPos: 579853
depth: 27  wins: 1219068  loses: 682241  newPos: 682240


-}

buildTableInteractive :: Game pos => IO (Map.Map pos Int, Map.Map pos Int)
buildTableInteractive = helper 0 (wrap endWins) (wrap endLoses) (Set.fromList endLoses)  where
    wrap = Map.fromList . map (\p -> (p, 0))
    helper d w l p = if null p 
                     then return (w, l)
                     else
                       do
                         let (w', l', p') = step d w l p
                         putStrLn $ "depth: " ++ show d ++ "  wins: " ++ show (length w') ++ "  loses: " ++ show (length l')  ++ "  newPos: " ++ show (length p')  
                         helper (d+1) w' l' p'


depth = 16
table = (buildTable depth) :: (M.Map Board Int, M.Map Board Int)
longestLoses =  filter ( (==(depth-1)) . snd) $ (M.toList  $ fst table)


mainFunc :: IO ()
mainFunc = const () <$> buildInteractiveChess --printBoard . fst $ (longestLoses !! 0)


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
          $ emptyBoard {getMoveSide = Black}
          
          
lastBoard = placeFigure (Just (Fig King Black)) (Coords 7 4)
          . placeFigure (Just (Fig King White)) (Coords 7 5)
          . placeFigure (Just (Fig Knight White)) (Coords 7 6)
          . placeFigure (Just (Fig Bishop White)) (Coords 7 7)
          $ emptyBoard {getMoveSide = Black}
          
          

loses :: [Board]
loses = do
         kw <- allFields
         kb <- nextFields kw
         let pos =   placeFigure (Just (Fig King White)) kw 
                   . placeFigure (Just (Fig King Black)) kb
                     $ emptyBoard {getMoveSide = Black}
         guard $ posIsValid pos
         b <- nextFields kb
         let posB = placeFigure (Just (Fig Bishop White)) b pos
         k <- nextFields b
         let posKB = placeFigure (Just (Fig Knight White)) k posB
         guard $ isMate posKB --450020
         return posKB


newtype BoardInt = BoardInt {getBoardInt :: Int} deriving (Show, Eq, Ord)

instance Game BoardInt where
  moves    = map (BoardInt . fromEnum) . availMovesBoard . toEnum . getBoardInt             
  preMoves = map (BoardInt . fromEnum) . prevMovesBoard . toEnum . getBoardInt
  endLoses = map (BoardInt . fromEnum) [mateBoard]
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


                  
