module Main where
import Data.List

type Inst = (String, Int, Bool)

main :: IO()
main = do
  contents <- getContents
  putStrLn $ solve $ lines contents

solve :: [String] -> String
solve input = show $ simulate instructions 0 0
  where instructions = map formatInst input

formatInst :: String -> Inst
formatInst i = (inst, num, False)
  where i' = separate ' ' i
        inst = i' !! 0
        num = sign * (read $ tail $ i' !! 1)
        sign = if (head (i' !! 1) == '+') then 1 else -1

simulate :: [Inst] -> Int -> Int -> Int
simulate list acc pos
  | visited = acc
  | ins == "nop" = simulate newlist acc (pos+1)
  | ins == "acc" = simulate newlist (acc+num) (pos+1)
  | ins == "jmp" = simulate newlist acc (pos+num)
  | otherwise = -1
  where (ins, num, visited) = list !! pos
        newlist = take pos list ++ [(ins, num, True)] ++ drop (pos + 1) list

separate :: (Eq a) => a -> [a] -> [[a]]
separate delim list = let (start, end) = break (== delim) list
                    in start : if null end then [] else separate delim (tail end)
