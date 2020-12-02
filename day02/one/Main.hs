module Main where

main :: IO()
main = do
  contents <- getContents
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ sum $ map checkRule $ map format input

checkRule :: (Int, Int, Char, String) -> Int
checkRule (min, max, letter, pw)
  | (min <= count && count <= max) = 1
  | otherwise = 0
  where count = sum $ map (\c -> if c == letter then 1 else 0) pw

-- This simply show's that I'm apparently too lazy for regexes,
-- it doesn't even work well with multiple delimiters.
split :: String -> Char -> (String, String)
split str delim = (takeWhile (/= delim) str, drop 1 (dropWhile (/= delim) str))
format :: String -> (Int, Int, Char, String)
format line = (read min, read max, head letter, drop 1 pw)
  where (rule, pw) = split line ':'
        (minmax, letter) = split rule ' '
        (min, max) = split minmax '-'
