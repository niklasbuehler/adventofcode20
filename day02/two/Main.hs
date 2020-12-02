module Main where

main :: IO()
main = do
  contents <- getContents
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ sum $ map checkRule $ map format input

checkRule :: (Int, Int, Char, String) -> Int
checkRule (pos1, pos2, letter, pw)
  | xor (pw !! (pos1-1) == letter) (pw !! (pos2-1) == letter) = 1
  | otherwise = 0
  where xor a b = (a || b) && not (a && b)

-- This simply show's that I'm apparently too lazy for regexes,
-- it doesn't even work well with multiple delimiters.
format :: String -> (Int, Int, Char, String)
format line = (read pos1, read pos2, head letter, drop 1 pw)
  where split :: String -> Char -> (String, String)
        split str delim = (takeWhile (/= delim) str, drop 1 (dropWhile (/= delim) str))
        (rule, pw) = split line ':'
        (positions, letter) = split rule ' '
        (pos1, pos2) = split positions '-'
