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
    addRule rmap (pattern, out) = Map.insert pattern out rmap

nextGeneration :: RulesMap -> (Int, Pots) -> (Int, Pots)
nextGeneration rmap (start, pots) = (start-2, nextGeneration' (False:False:False:False:pots))
  where
    nextGeneration' :: Pots -> Pots
    nextGeneration' [] = []
    nextGeneration' (d:[]) = ((rmap Map.! (d,False,False,False,False)):nextGeneration' [])
    nextGeneration' (c:d:[]) = ((rmap Map.! (c,d,False,False,False)):nextGeneration' (d:[]))
    nextGeneration' (b:c:d:[]) = ((rmap Map.! (b,c,d,False,False)):nextGeneration' (c:d:[]))
    nextGeneration' (b:c:d:e:[]) = ((rmap Map.! (b,c,d,e,False)):nextGeneration' (c:d:e:[]))
    nextGeneration' (a:b:c:d:e:ps) = ((rmap Map.! (a,b,c,d,e)):nextGeneration' (b:c:d:e:ps))

nextNGeneration :: RulesMap -> Int -> (Int, Pots) -> (Int, Pots)
nextNGeneration rmap 0 (s, ps) = (s, ps)
nextNGeneration rmap n (s, ps) = nextNGeneration rmap (n-1) (nextGeneration rmap (s,ps))

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
