module Day2 (solve) where
import Utils
import Control.Monad

data ProductId = ProductId { first :: String
                           , last  :: String
                           } deriving (Show, Eq)

getInvalidIds :: ProductId -> [Int]
getInvalidIds (ProductId f l) = filter checkId idRange
    where idRange   = [read f .. read l + 1]
          checkId i = let idString = show i
                          (a, b)   = splitAt (length idString `div` 2) idString
                       in a == b

solve :: String -> String
solve = show . sum . (parse >=> getInvalidIds)

parse :: String -> [ProductId]
parse = fmap (createId . splitOn '-') . splitOn ','
        where createId [f, l] = ProductId f l

