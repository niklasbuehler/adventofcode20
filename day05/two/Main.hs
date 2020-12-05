module Main where

main :: IO()
main = do
  contents <- getContents
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ findId ids
  where positions = map parse input
        ids = map (\(r,c) -> r * 8 + c) positions

findId :: [Int] -> Int
findId ids = head [ b | a <- ids, c <- ids, b <- empty, c == a+2, b == a+1 ]
  where empty = filter (\e -> not $ elem e ids) [1..maxid]
        maxid = 127 * 8 + 7

parse :: String -> (Int, Int)
parse seat = (convert (substRow row), convert (substCol col))
  where (row, col) = (take 7 seat, drop 7 seat)
        substRow = map (\c -> if c == 'F' then '0' else '1')
        substCol = map (\c -> if c == 'L' then '0' else '1')

-- Convert binary strings to integers
convert :: String -> Int
convert str = convert' $ reverse str
  where convert' "" = 0
        convert' (x:xs) = (read $ x:"") + 2 * convert' xs
