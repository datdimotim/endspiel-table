module ChessPrint where

import ChessTypes

import Data.Char (toUpper)
import Data.Array (Array, Ix, listArray, (!), (//), accumArray)

replacePlaceholders :: Char -> String -> [String] -> String
replacePlaceholders _ [] _ = []
replacePlaceholders c (t:ts) r = if t == c
                                      then head r ++ replacePlaceholders c ts (tail r)
                                      else t : replacePlaceholders c ts r

                            
printFig :: Field -> String
printFig Nothing = " "
printFig (Just (Fig t Black)) = printFigType t
printFig (Just (Fig t White)) = map toUpper $ printFigType t

printFigType :: FigType -> String
printFigType Pawn = "p"
printFigType Rook = "r"
printFigType Knight = "n"
printFigType Bishop = "b"
printFigType Queen = "q"
printFigType King = "k"


printBoard :: Board -> IO ()
printBoard = putStrLn . formatBoard

formatBoard :: Board -> String
formatBoard board = let
                              b = getFields board
                              ms = getMoveSide board
                              d = do
                                    n <- reverse [0..7]
                                    l <- [0..7]
                                    let c = Coords l n
                                    let field = accumArray (flip const) Nothing (Coords 0 0, Coords 7 7) (map (\(c, f) -> (c, Just f)) b)
                                    let f = field ! c
                                    return $ printFig f 
                           in 
                              replacePlaceholders '*' boardTemplate (d ++ [show ms])


boardTemplate :: String
boardTemplate = "    _____ _____ _____ _____ _____ _____ _____ _____\r\n   |     |     |     |     |     |     |     |     |\r\n 8 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |\r\n   |_____|_____|_____|_____|_____|_____|_____|_____|\r\n   |     |     |     |     |     |     |     |     |\r\n 7 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |\r\n   |_____|_____|_____|_____|_____|_____|_____|_____|\r\n   |     |     |     |     |     |     |     |     |\r\n 6 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |\r\n   |_____|_____|_____|_____|_____|_____|_____|_____|\r\n   |     |     |     |     |     |     |     |     |\r\n 5 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |\r\n   |_____|_____|_____|_____|_____|_____|_____|_____|\r\n   |     |     |     |     |     |     |     |     |\r\n 4 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |\r\n   |_____|_____|_____|_____|_____|_____|_____|_____|\r\n   |     |     |     |     |     |     |     |     |\r\n 3 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |\r\n   |_____|_____|_____|_____|_____|_____|_____|_____|\r\n   |     |     |     |     |     |     |     |     |\r\n 2 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |\r\n   |_____|_____|_____|_____|_____|_____|_____|_____|\r\n   |     |     |     |     |     |     |     |     |\r\n 1 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |\r\n   |_____|_____|_____|_____|_____|_____|_____|_____|\r\n      A     B     C     D     E     F     G     H\r\n                   move: *\r\n      "

{-
    
    _____ _____ _____ _____ _____ _____ _____ _____
   |     |     |     |     |     |     |     |     |
 8 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |
   |_____|_____|_____|_____|_____|_____|_____|_____|
   |     |     |     |     |     |     |     |     |
 7 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |
   |_____|_____|_____|_____|_____|_____|_____|_____|
   |     |     |     |     |     |     |     |     |
 6 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |
   |_____|_____|_____|_____|_____|_____|_____|_____|
   |     |     |     |     |     |     |     |     |
 5 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |
   |_____|_____|_____|_____|_____|_____|_____|_____|
   |     |     |     |     |     |     |     |     |
 4 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |
   |_____|_____|_____|_____|_____|_____|_____|_____|
   |     |     |     |     |     |     |     |     |
 3 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |
   |_____|_____|_____|_____|_____|_____|_____|_____|
   |     |     |     |     |     |     |     |     |
 2 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |
   |_____|_____|_____|_____|_____|_____|_____|_____|
   |     |     |     |     |     |     |     |     |
 1 |  *  |  *  |  *  |  *  |  *  |  *  |  *  |  *  |
   |_____|_____|_____|_____|_____|_____|_____|_____|
      A     B     C     D     E     F     G     H
                   move: *


-}




