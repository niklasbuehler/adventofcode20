module Main where

main :: IO()
main = do
  contents <- getContents
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ sum $ map ((\b -> if b then 1 else 0) . check) passports
  where passports = condense input

-- Condense all lines separated by empty lines into passport-bundles
condense :: [String] -> [String]
condense [] = []
condense list = (concat $ map (++" ") passport) : (condense $ drop 1 rest)
  where (passport, rest) = break (== "") list

-- Check whether all required fields are given in a passport string
check :: String -> Bool
check line = (length $ filter (== True) checks) == 7
  where checks = map (\f -> elem f fields) required
        required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        fields = map (\str -> fst $ break (== ':') str) $ groupBy line ' '

groupBy :: String -> Char -> [String]
groupBy str delim = let (start, end) = break (== delim) str
                    in start : if null end then [] else groupBy (tail end) delim
