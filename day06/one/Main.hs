module Main where
import Data.List

main :: IO()
main = do
  contents <- getContents
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ sum $ map length groups
  where groups = map (uniq . sort . concat) $ formGroups "" input

formGroups :: (Eq a) => a -> [a] -> [[a]]
formGroups delim list = let (start, end) = break (== delim) list
                    in start : if null end then [] else formGroups delim (tail end)

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq list
  | length list == 1 = list
  | otherwise = if (x == head xs) then uniq xs else x : uniq xs
  where x = head list
        xs = tail list
