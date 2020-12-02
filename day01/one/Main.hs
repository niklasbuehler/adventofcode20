module Main where

main :: IO()
main = do  
  contents <- getContents  
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = (show x) ++ "+" ++ (show y) ++ "=" ++ (show (x+y))
  where numbers = map read input
        (x,y) = head pairs
        pairs = [ (a,b) | a <- numbers, b <- numbers, a+b==2020]
