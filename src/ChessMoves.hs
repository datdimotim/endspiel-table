{-# Language BangPatterns #-}

module ChessMoves where

import Control.Monad (guard)
import Data.Maybe (fromMaybe, isNothing)
import Data.Foldable (toList)
import Data.List (find)

import ChessTypes

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

