import System.Environment
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as Map

type Pots = [Bool]
type Pattern = (Bool, Bool, Bool, Bool, Bool)
type Rule = (Pattern, Bool)
type RulesMap = Map.Map Pattern Bool

parseInput :: String -> (Pots, RulesMap)
parseInput s = (state, rulesMap rules)
  where
    ls = lines s
    state = map toBool (drop 15 (head ls))
    rules = map toRule (map (splitOn " => ") (drop 2 ls))

    toBool :: Char -> Bool
    toBool '#' = True
    toBool '.' = False

    toRule :: [String] -> Rule
    toRule (pattern:out:[]) = (toPattern pattern, toBool (head out))

    toPattern :: [Char] -> Pattern
    toPattern (a:b:c:d:e:[]) = (toBool a, toBool b, toBool c, toBool d, toBool e)

rulesMap :: [Rule] -> RulesMap
rulesMap rules = foldl addRule Map.empty rules
  where
    addRule :: RulesMap -> Rule -> RulesMap
    addRule rmap (pattern, out) = if out
      then Map.insert pattern out rmap
      else rmap

patternFromList :: [Bool] -> (Bool, Bool, Bool, Bool, Bool)
patternFromList (a:b:c:d:e:xs)  = (a, b, c, d, e)
patternFromList (a:b:c:d:xs)    = (a, b, c, d, False)
patternFromList (a:b:c:xs)      = (a, b, c, False, False)
patternFromList (a:b:xs)        = (a, b, False, False, False)
patternFromList (a:xs)          = (a, False, False, False, False)
patternFromList []              = (False, False, False, False, False)

nextGeneration :: RulesMap -> (Int, Pots) -> (Int, Pots)
nextGeneration rmap (start, pots) = trimBegin (start-1, nextGeneration' (False:False:False:pots))
  where
    nextGeneration' :: Pots -> Pots
    nextGeneration' [] = []
    nextGeneration' pots = ((getV (patternFromList pots)):nextGeneration' (tail pots))

    getV :: Pattern -> Bool
    getV p = Map.findWithDefault False p rmap

    trimBegin :: (Int, Pots) -> (Int, Pots)
    trimBegin (n, (False:pots)) = trimBegin (n+1,pots)
    trimBegin x = x

equalPots :: Pots -> Pots -> Bool
equalPots (x:xs) (y:ys) = (x == y) && (equalPots xs ys)
equalPots [] xs = foldl (\acc v -> acc && (v == False)) True xs
equalPots xs [] = foldl (\acc v -> acc && (v == False)) True xs

nextNGeneration :: RulesMap -> Int -> (Int, Pots) -> (Int, Pots)
nextNGeneration rmap 0 (s, ps) = (s, ps)
nextNGeneration rmap n (s, ps) = nextNGeneration' (n-1) (s, ps) (nextGeneration rmap (s,ps))
  where
    nextNGeneration' :: Int -> (Int, Pots) -> (Int, Pots) -> (Int, Pots)
    nextNGeneration' 0 prev current = current
    nextNGeneration' m' (prevPos, prevGen) current@(currPos, currGen) = if equalPots prevGen currGen
      then (currPos + (currPos - prevPos) * m', currGen)
      else nextNGeneration' (m'-1) current (nextGeneration rmap current)

sumPotsNumbers :: (Int, Pots) -> Int
sumPotsNumbers (_, []) = 0
sumPotsNumbers (i, (p:ps)) = if p
  then i + sumPotsNumbers (i+1, ps)
  else sumPotsNumbers (i+1, ps)

main :: IO ()
main = do
  s <- readFile "input/day-12.input"
  let (pots, rMap) = parseInput s
  print $ sumPotsNumbers (nextNGeneration rMap 20 (0, pots))
  print $ sumPotsNumbers (nextNGeneration rMap 50000000000 (0, pots))
