{-# LANGUAGE LambdaCase #-}
module Day1 (solve) where

data Rotation = L Int
              | R Int
              deriving (Show, Eq)

wrap :: Int -> Int
wrap n = n `mod` 100

data Lock = Lock { position :: Int
                 , clicks   :: Int
                 } deriving (Show, Eq)

rotate :: Lock -> Rotation -> Lock

rotate lock (L n) = let pos       = (wrap (100 - position lock) + n)
                        newClicks = (pos `div` 100)
                     in Lock (wrap (100 - wrap pos)) (clicks lock + newClicks)

rotate lock (R n) = let pos       = position lock + n
                        newClicks = (pos `div` 100)
                     in Lock (wrap pos) (clicks lock + newClicks)

solve :: String -> String
solve input = let rotations = parse input
                  lock      = foldl rotate (Lock 50 0) rotations
               in show (clicks lock)

parse :: String -> [Rotation]
parse input = fmap (\case
                        ('L':num) -> L (read num)
                        ('R':num) -> R (read num)
                   ) (lines input)

