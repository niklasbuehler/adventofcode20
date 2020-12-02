module Main where

main :: IO()
main = do  
  contents <- getContents  
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = (show x) ++ "+" ++ (show y) ++ "+" ++ (show z) ++ "=" ++ (show (x+y+z))
  where numbers = map read input
        (x,y,z) = head triples
        triples = [ (a,b,c) | a <- numbers, b <- numbers, c <- numbers, a+b+c==2020]
