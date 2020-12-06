module Main where

main :: IO()
main = do
  contents <- getContents
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ sum $ map check groups
  where groups = formGroups "" input

check :: [String] -> Int
check (a:as) = sum $ map (\b -> if b then 1 else 0) $ map inAllOthers a
  where inAllOthers c = and $ map (elem c) as

formGroups :: (Eq a) => a -> [a] -> [[a]]
formGroups delim list = let (start, end) = break (== delim) list
                    in start : if null end then [] else formGroups delim (tail end)
