{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TupleSections, InstanceSigs, ScopedTypeVariables, BangPatterns, DeriveGeneric #-}

module ChessTypes (
  FigType(..),
  Color(..),
  invColor,
  Fig(..),
  Field,
  Coords(..),
  Offset(..),
  Board,
  mkBoard,
  getFields,
  getMoveSide,
  mapFields,
  mapMoveSide
) where


import Text.Read hiding (step)
import Control.Monad
import Data.List (nub, sort)
import Data.Foldable (toList, foldl')
import Data.Array (Array, Ix, listArray, (!), (//), assocs, accumArray)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..), getSum, Endo(..), appEndo)

import GHC.Generics
import Control.DeepSeq



data FigType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

data Color = White | Black deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

invColor :: Color -> Color
invColor White = Black
invColor Black = White

data Fig = Fig { getFigType :: !FigType 
               , getColor :: !Color
               } deriving (Eq, Ord, Show, Read, Generic)
               
type Field = Maybe Fig
data Coords = Coords { getLetter :: !Int
                     , getNumber  :: !Int
                     } deriving (Show, Eq, Ord, Ix, Read, Generic)
                     
instance NFData Coords where         
instance NFData Fig where
instance NFData FigType where   
instance NFData Color where   

instance Enum Fig where
  toEnum = uncurry Fig . toEnum 
  fromEnum (Fig f c) = fromEnum (f, c)
                         

instance (Enum a, Bounded b, Enum b) => Enum (a, b) where
  toEnum :: forall  a b . (Enum a, Bounded b, Enum b) => Int -> (a, b)
  toEnum n = let
               max = 1 + fromEnum (maxBound :: b)
             in
               (toEnum $ n `div` max, toEnum $ n `mod` max)
  
  fromEnum :: forall  a b . (Enum a, Bounded b, Enum b) => (a, b) -> Int
  fromEnum (a, b) = let
                     max = 1 + fromEnum (maxBound :: b)
                   in
                     fromEnum a * max + fromEnum b
                                              

instance (Bounded a, Enum a) => Enum [a] where
  toEnum :: forall  a . (Bounded a, Enum a) => Int -> [a]
  toEnum 0 = []
  toEnum n = let
               max = 1 + fromEnum (maxBound :: a)
               (len, rem) = let
                              l n a | max ^ a > n = (a, n)
                                    | otherwise     = l (n - (max ^ a)) (a+1)
                            in
                              l n 0
                       
               signPart = let
                            g n | n == 0    = []
                                | otherwise = toEnum (n `mod` max) : g (n `div` max)
                          in
                            g rem
                           
               signLen  = length signPart
               addCount = len - signLen 
             in
               reverse $ signPart ++ replicate addCount minBound
               
  fromEnum :: forall  a . (Bounded a, Enum a) => [a] -> Int
  fromEnum l = let
               max = 1 + fromEnum (maxBound :: a)
               offset = getSum $ foldMap (Sum . (^) max) [0 .. length l - 1]
               ff :: Int -> Int -> Int
               ff e = \a -> a * max + fromEnum e
               ind = ($ 0) . appEndo . foldMap (Endo . ff . fromEnum) $ reverse l
             in
               offset + ind

                         
instance Bounded Fig where
  maxBound = Fig maxBound maxBound
  minBound = Fig minBound minBound
  
                     
instance Enum Coords where
  toEnum n = Coords (n `div` 8) (n `mod` 8)
  fromEnum (Coords l n) = l * 8  + n
  
instance Bounded Coords where
  minBound = Coords 0 0
  maxBound = Coords 7 7
  
                     
data Offset = Offset { getDLetter :: Int
                     , getDNumber  :: Int
                     }


data Board = Board [(Coords, Fig)] Color

mkBoard :: [(Coords, Fig)] -> Color -> Board
mkBoard f !mc = Board f mc

getFields :: Board -> [(Coords, Fig)]
getFields (Board f c) = f

getMoveSide :: Board -> Color
getMoveSide (Board f c) = c

mapFields :: ([(Coords, Fig)] -> [(Coords, Fig)]) -> Board -> Board
mapFields f (Board fs c) = mkBoard (f fs) c

mapMoveSide :: (Color -> Color) -> Board -> Board
mapMoveSide f (Board fs c) = Board fs (f c)

instance Enum Board where
  toEnum = uncurry mkBoard . toEnum
  fromEnum (Board fgs ms) = fromEnum (sort fgs, ms)

                  
