module Day2 (solve) where
import Utils
import Control.Monad

data ProductId = ProductId { first :: String
                           , last  :: String
                           } deriving (Show, Eq)

getInvalidIds :: ProductId -> [Int]
getInvalidIds (ProductId f l) = filter checkId idRange
    where idRange   = [read f .. read l + 1]
          checkId i = let idString   = show i
                          maxSeqLen  = length idString `div` 2
                          repeatSeqs = map (`take` idString)
                                     $ filter ((==0) . (length idString `mod`))
                                              [1..maxSeqLen]
                       in any (all (uncurry (==)) . zip idString . cycle) repeatSeqs

solve :: String -> String
solve = show . sum . (parse >=> getInvalidIds)

parse :: String -> [ProductId]
parse = fmap (createId . splitOn '-') . splitOn ','
        where createId [f, l] = ProductId f l

