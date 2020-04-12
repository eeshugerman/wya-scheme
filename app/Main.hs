module Main where
import System.Environment

stringSum :: [String] -> String
stringSum args = show $ sum $ map read args

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Sum: " ++ stringSum args)
