module Main where
import System.Environment
import Day2

main :: IO ()
main = do
        path <- getEnv "AOC_2025_INPUTS"
        contents <- readFile (path ++ "/day2")
        let result = solve contents
        putStrLn result

