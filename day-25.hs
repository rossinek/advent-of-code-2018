import System.Environment
import Data.List (partition)
import Data.List.Split (splitOn)
-- import Data.List.Extra (maximumOn)
-- import Data.Maybe
import qualified Data.Set as Set
-- import qualified Data.Map.Lazy as Map

type Constelation = Set.Set Point
type Point = (Int, Int, Int, Int)

parseInput :: String -> [Point]
parseInput s = map parsePoint (lines s)
  where
    parsePoint :: String -> Point
    parsePoint s = let (t:x:y:z:_) = map read (splitOn "," s) in (t,x,y,z)

dist :: Point -> Point -> Int
dist (t,x,y,z) (t',x',y',z') = (abs (t - t')) + (abs (x - x')) + (abs (y - y')) + (abs (z - z'))

groupConstelations :: [Point] -> [Constelation]
groupConstelations points = foldl addPoint [] points

addPoint :: [Constelation] -> Point -> [Constelation]
addPoint constelations p = ((foldl Set.union (Set.singleton p) partOf):notPartOf)
  where
    (partOf, notPartOf) = partition (belongsTo p) constelations

belongsTo :: Point -> Constelation -> Bool
belongsTo p constelation = any (\p' -> dist p p' <= 3) (Set.elems constelation)

main :: IO ()
main = do
  s <- readFile "input/day-25.input"
  let input = parseInput s
  print $ length (groupConstelations input)
