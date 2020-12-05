module Main where
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

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
  where checks = map (\(k,v) -> elem k required) validfields
        validfields = filter isValid fields
        fields = map (break (== ':')) $ groupBy line ' '
        required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- Check whether a single field is valid
isValid :: (String, String) -> Bool
isValid (k,v')
  | k == "byr" = v =~ "^[0-9]{4}$" && (between v 1920 2002)
  | k == "iyr" = v =~ "^[0-9]{4}$" && (between v 2010 2020)
  | k == "eyr" = v =~ "^[0-9]{4}$" && (between v 2020 2030)
  | k == "hgt" = (v =~ "^[0-9]{3}cm$" && between (removeUnit v) 150 193) || (v =~ "^[0-9]{2}in$" && between (removeUnit v) 59 76)
  | k == "hcl" = v =~ "^#[0-9a-f]{6}$"
  | k == "ecl" = elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  | k == "pid" = v =~ "^[0-9]{9}$"
  | k == "cid" = True
  | otherwise = False
  where v = tail v'
        between str min max = (read str) >= min && (read str) <= max
        removeUnit = (init . init)

groupBy :: String -> Char -> [String]
groupBy str delim = let (start, end) = break (== delim) str
                    in start : if null end then [] else groupBy (tail end) delim
