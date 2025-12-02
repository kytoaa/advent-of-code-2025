module Main where
import System.Environment
import Day1

main :: IO ()
main = do
        path <- getEnv "AOC_2025_INPUTS"
        contents <- readFile (path ++ "/day1")
        let result = solve contents
        putStrLn result

