import System.Environment
import Data.Set (Set, member, empty, insert)

parseInput :: String -> [Int]
parseInput s = map (read . skipPlus) (lines s)

skipPlus :: String -> String
skipPlus ('+':xs) = xs
skipPlus xs = xs

frequency :: [Int] -> Int
frequency [] = 0
frequency (n:ns) = n + frequency ns

calibrate :: [Int] -> Int
calibrate ns = calibrate' empty 0 ns
  where
    calibrate' :: Set Int -> Int -> [Int] -> Int
    calibrate' visited freq [] = calibrate' visited freq ns
    calibrate' visited freq (x:xs) = if freq `member` visited
      then freq
      else calibrate' (insert freq visited) (freq + x) xs

main :: IO ()
main = do
  s <- readFile "input/day-01.input"
  let input = parseInput s
  print $ frequency input
  print $ calibrate input

