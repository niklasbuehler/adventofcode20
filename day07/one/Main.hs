module Main where
import Data.List

main :: IO()
main = do
  contents <- getContents
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ length $ containersOf rules [] "shinygold"
  where rulesStrings = map (break (== "bags") . separate ' ') input
        rules = map (\(r1,r2) -> (concat r1, \s -> isInfixOf s (concat r2))) rulesStrings

-- Containers are stored as (String, String -> Bool),
-- where fst is the color of the container, snd is the rule.
-- Uses acc to break up cycles.
containersOf :: [(String, String -> Bool)] -> [String] -> String -> [String]
containersOf rules acc col = merge containers containers'
  where containers = map fst $ filter isNewContainer rules
        containers' = concat $ map (containersOf rules (merge acc containers)) containers -- Recurse but avoid running into cycles
        isNewContainer (color, contains) = not (elem color acc) && (contains col)

 -- Disjoint merge
merge :: (Ord a) => [a] -> [a] -> [a]
merge as bs = uniq $ sort (as ++ bs)

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq list
  | length list == 1 = list
  | otherwise = if (x == head xs) then uniq xs else x : uniq xs
  where x = head list
        xs = tail list

separate :: (Eq a) => a -> [a] -> [[a]]
separate delim list = let (start, end) = break (== delim) list
                    in start : if null end then [] else separate delim (tail end)
