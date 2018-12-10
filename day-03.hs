import System.Environment
import Data.List.Split
import Data.List (find)
import qualified Data.Map.Lazy as Map

type Fabric = (ID, Point, Size)
type Point = (Int, Int)
type Size = (Int, Int)
type ID = Int
type FabricsMap = Map.Map Point [ID]

parseInput :: String -> [Fabric]
parseInput s = map parseFabric (lines s)
  where
    parseFabric :: String -> Fabric
    parseFabric s' = let
        (i:_:pos:size:_) = words s'
        id = read (tail i)
        (x:y:_) = map read (splitOn "," (init pos))
        (w:h:_) = map read (splitOn "x" size)
      in (id, (x, y), (w, h))

fabricsMap :: [Fabric] -> FabricsMap
fabricsMap [] = Map.empty
fabricsMap (f:fs) = Map.unionWith (++) (fabricMap f) (fabricsMap fs)
  where
    fabricMap :: Fabric -> FabricsMap
    fabricMap (id, (x,y), (w,h)) = Map.fromList [((x',y'), [id]) | x' <- [x..(x+w-1)], y' <- [y..(y+h-1)]]

countOverlapping :: FabricsMap -> Int
countOverlapping fm = Map.size (Map.filter atLeastTwo fm)
  where
    atLeastTwo :: [a] -> Bool
    atLeastTwo (_:_:_) = True
    atLeastTwo _ = False

notOverlapping :: FabricsMap -> Fabric -> Bool
notOverlapping fm (_, (x,y), (w,h)) = foldl (\acc -> \p -> acc && hasOneIndex p) True indices
  where
    indices = [(x',y') | x' <- [x..(x+w-1)], y' <- [y..(y+h-1)]]
    hasOneIndex :: Point -> Bool
    hasOneIndex p = (length (fm Map.! p)) == 1

main :: IO ()
main = do
  s <- readFile "input/day-03.input"
  let fabrics = parseInput s
  let mapOfFabrics = fabricsMap fabrics
  print $ countOverlapping mapOfFabrics
  let Just (notOverlappingId,_,_) = find (notOverlapping mapOfFabrics) fabrics
  print $ notOverlappingId
