module Main where

main :: IO()
main = do  
  contents <- getContents  
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ product $ map (\(x,y) -> step x y field 0) steps
  where field = map cycle input
        steps = [(1,1),(3,1),(5,1),(7,1),(1,2)]

step :: Int -> Int -> [String] -> Int -> Int
step _ _ [] i = i
step x y field acc = step x y field' (acc+tree)
  where tree = if (head $ head field) == '#' then 1 else 0
        field' = drop y $ map (drop x) field
