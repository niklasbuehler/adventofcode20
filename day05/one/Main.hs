module Main where

main :: IO()
main = do
  contents <- getContents
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ maximum $ map (\(r,c) -> r * 8 + c) positions
  where positions = map parse input

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
