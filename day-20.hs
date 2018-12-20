import System.Environment
import qualified Data.Set as Set

type Edge = (Vertex, Vertex)
type Vertex = (Int, Int)
type EdgeSet = Set.Set Edge

parseInput :: String -> [String]
parseInput s = regexGenerate (init (tail (head (lines s))))

regexGenerate :: String -> [String]
regexGenerate [] = [""]
regexGenerate rx@('(':xs) = shiftToAll inside (regexGenerate rest)
  where
    (inside, rest) = consumeBrackets rx
regexGenerate (a:xs) = shiftToAll [[a]] (regexGenerate xs)

shiftToAll :: [String] -> [String] -> [String]
shiftToAll [] _ = []
shiftToAll (a:as) strs = (map (a ++) strs) ++ (shiftToAll as strs)

consumeBrackets :: String -> ([String], String)
consumeBrackets ('(':ts) = consumeBrackets' [""] 1 ts
  where
    consumeBrackets' :: [String] -> Int -> String -> ([String], String)
    consumeBrackets' (l:ls) 1 (')':ts) = (((regexGenerate (reverse l)) ++ ls), ts)
    consumeBrackets' (l:ls) 1 ('|':ts) = consumeBrackets' (("":(regexGenerate (reverse l))) ++ ls) 1 ts
    consumeBrackets' (l:ls) n (')':ts) = consumeBrackets' ((')':l):ls) (n-1) ts
    consumeBrackets' (l:ls) n ('(':ts) = consumeBrackets' (('(':l):ls) (n+1) ts
    consumeBrackets' (l:ls) n (c:ts) = consumeBrackets' ((c:l):ls) n ts

main :: IO ()
main = do
  s <- readFile "input/day-20.input"
  print $ length (parseInput s)
