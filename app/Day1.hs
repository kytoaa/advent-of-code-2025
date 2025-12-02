{-# LANGUAGE LambdaCase #-}
module Day1 (solve) where

data Rotation = L Int
              | R Int
              deriving (Show, Eq)

wrap :: Int -> Int
wrap n = mod n 100

rotate :: Int -> Rotation -> Int
rotate x (L n) = wrap (x - n)
rotate x (R n) = wrap (x + n)

solve :: String -> String
solve input = let rotations = parse input
                  positions = scanl rotate 50 rotations
               in (show . length . filter (==0)) positions

parse :: String -> [Rotation]
parse input = fmap (\case
                        ('L':num) -> L (read num)
                        ('R':num) -> R (read num)
                   ) (lines input)
