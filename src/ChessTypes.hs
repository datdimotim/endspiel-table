{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TupleSections, InstanceSigs, ScopedTypeVariables #-}

module ChessTypes where


import Text.Read hiding (step)
import Control.Monad
import Data.List (nub)
import Data.Foldable (toList)
import Data.Array (Array, Ix, listArray, (!), (//), assocs, accumArray)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..), getSum, Endo(..), appEndo)



data FigType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Color = White | Black deriving (Eq, Ord, Show, Read, Enum, Bounded)

invColor :: Color -> Color
invColor White = Black
invColor Black = White

data Fig = Fig { getFigType :: FigType 
               , getColor :: Color
               } deriving (Eq, Ord, Show, Read)
               
type Field = Maybe Fig
data Coords = Coords { getLetter :: Int
                     , getNumber  :: Int
                     } deriving (Show, Eq, Ord, Ix, Read)
                     
                     

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
                                              


pow :: Int -> Int -> Int
pow a 0 = 1
pow a n = a * pow a (n-1)

instance (Bounded a, Enum a) => Enum [a] where
  toEnum :: forall  a . (Bounded a, Enum a) => Int -> [a]
  toEnum 0 = []
  toEnum n = let
               max = 1 + fromEnum (maxBound :: a)
               (len, rem) = let
                              l n a | pow max a > n = (a, n)
                                    | otherwise     = l (n - pow max a) (a+1)
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
               reverse $ signPart ++ take addCount (repeat minBound)
               
  fromEnum :: forall  a . (Bounded a, Enum a) => [a] -> Int
  fromEnum l = let
               max = 1 + fromEnum (maxBound :: a)
               offset = getSum $ foldMap (Sum . pow max) [0 .. length l - 1]
               ff :: Int -> Int -> Int
               ff e = \a -> a * max + (fromEnum e)
               ind = ($ 0) . appEndo . foldMap (\e -> Endo $ ff (fromEnum e)) $ reverse l
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


data Board = Board { getFields   :: Array Coords Field
               , getMoveSide :: Color
               } deriving (Eq, Ord, Show, Read)
               


instance Enum Board where
  toEnum n = let
               (fgs :: [(Coords, Fig)], ms :: Color) = toEnum n
               bounds = ((Coords 0 0), (Coords 7 7))
               field = accumArray (flip const) Nothing bounds (map (\(c, f) -> (c, Just f)) fgs)
             in
               Board field ms
  fromEnum (Board field ms) = let
                                fgs :: [(Coords, Fig)]
                                fgs = concatMap (toList . (\(c, f) -> (c,) <$> f)) . assocs $ field
                              in 
                                fromEnum (fgs, ms)

                  
