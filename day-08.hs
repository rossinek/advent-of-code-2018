import System.Environment

type Licence = [Int]
type Metadata = [Int]

parseInput :: String -> Licence
parseInput s = map read (words s)

totalMetadata :: Licence -> Int
totalMetadata (nnodes:nmeta:tail) = fst (addNodeMeta nnodes nmeta tail)

addNodeMeta :: Int -> Int -> Licence -> (Int, Licence)
addNodeMeta 0 nmeta licence = (sum meta, rest)
  where
    (meta, rest) = splitAt nmeta licence
addNodeMeta nnodes nmeta (n:m:tail) = (childSum + restTotal, nextRest)
  where
    (childSum, rest) = addNodeMeta n m tail
    (restTotal, nextRest) = addNodeMeta (nnodes-1) nmeta rest

main :: IO ()
main = do
  s <- readFile "day-08.input"
  let input = parseInput s
  print $ totalMetadata input
