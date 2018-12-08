import System.Environment

type Licence = [Int]
type Metadata = [Int]
data LicenceTree a = LicenceNode a [LicenceTree a]

parseInput :: String -> Licence
parseInput s = map read (words s)

constructTree :: Licence -> LicenceTree Metadata
constructTree licence = fst (consumeNode licence)

consumeNode :: Licence -> (LicenceTree Metadata, Licence)
consumeNode (n:m:rest) = (LicenceNode (take m nextRest) children, drop m nextRest)
  where
    (children, nextRest) = consumeNodes n rest

consumeNodes :: Int -> Licence -> ([LicenceTree Metadata], Licence)
consumeNodes 0 l = ([], l)
consumeNodes n l = ((child:nextChildren), nextRest)
  where
    (child, rest) = consumeNode l
    (nextChildren, nextRest) = consumeNodes (n-1) rest

totalMetadataOnTree :: LicenceTree Metadata -> Int
totalMetadataOnTree (LicenceNode meta children) = (sum meta) + (sum (map totalMetadataOnTree children))

nodeValue :: LicenceTree Metadata -> Int
nodeValue (LicenceNode meta []) = sum meta
nodeValue (LicenceNode meta children) = sum $ map metaToValue meta
  where
    metaToValue :: Int -> Int
    metaToValue m = if length children >= m then nodeValue (children !! (m-1)) else 0

main :: IO ()
main = do
  s <- readFile "input/day-08.input"
  let input = parseInput s
  let tree = constructTree input
  print $ totalMetadataOnTree tree
  print $ nodeValue tree
