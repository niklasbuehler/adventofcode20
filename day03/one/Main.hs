module Main where

main :: IO()
main = do  
  contents <- getContents  
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ step field 0
  where field = map cycle input

step :: [String] -> Int -> Int
step [] i = i
step field acc = step field' (acc+tree)
  where tree = if (head $ head field) == '#' then 1 else 0
        field' = tail $ map (drop 3) field
