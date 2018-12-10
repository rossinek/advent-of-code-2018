import System.Environment
import qualified Data.Map.Lazy as Map

type ID = [Char]

parseInput :: String -> [ID]
parseInput s = lines s

hasTwoOrThree :: ID -> (Bool, Bool)
hasTwoOrThree s = hasTwoOrThree' Map.empty s
  where
    hasTwoOrThree' :: Map.Map Char Int -> [Char] -> (Bool, Bool)
    hasTwoOrThree' desc []  = ((Map.size exactlyTwo) > 0, (Map.size exactlyThree) > 0)
      where
        exactlyTwo = Map.filter (==2) desc
        exactlyThree = Map.filter (==3) desc
    hasTwoOrThree' desc (k:ks) = hasTwoOrThree' desc' ks
      where
        desc' = Map.insertWith (+) k 1 desc

checksum :: [ID] -> Int
checksum ids = checksum' (0, 0) ids
  where
    checksum' :: (Int, Int) -> [ID] -> Int
    checksum' (x, y) [] = x * y
    checksum' (x, y) (k:ks) = checksum' (x', y') ks
      where
        (h2, h3) = hasTwoOrThree k
        x' = if h2 then x + 1 else x
        y' = if h3 then y + 1 else y

differentChars :: ID -> ID -> Int
differentChars [] [] = 0
differentChars xs [] = length xs
differentChars [] xs = length xs
differentChars (x:xs) (y:ys) = if x == y
  then differentChars xs ys
  else 1 + (differentChars xs ys)

commonSequence :: ID -> ID -> String
commonSequence xs [] = []
commonSequence [] xs = []
commonSequence (x:xs) (y:ys) = if x == y
  then (x : commonSequence xs ys)
  else commonSequence xs ys

closePairCommonSequence :: [ID] -> String
closePairCommonSequence ids = head
  [ commonSequence id id' | id <- ids,
                            id' <- ids,
                            (differentChars id id') == 1 ]

main :: IO ()
main = do
  s <- readFile "input/day-02.input"
  let input = parseInput s
  print $ checksum input
  print $ closePairCommonSequence input

