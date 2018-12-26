import System.Environment
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.List

type Nanobot = (Position, Radius)
type Position = (Int, Int, Int)
type Radius = Int

parseInput :: String -> [Nanobot]
parseInput s = map parseNanobot (lines s)
  where
    parseNanobot :: String -> Nanobot
    parseNanobot ns = (pos, radius)
      where
        (p':r':_) = splitOn ", " ns
        (x:y:z:_) = splitOn "," (head (splitOn ">" (drop 5 p')))
        pos = (read x :: Int, read y :: Int, read z :: Int)
        radius = read (drop 2 r') :: Int

numberOfMaxInRange :: [Nanobot] -> Int
numberOfMaxInRange ns = length (filter (\(pos, _) -> (dist mpos pos) <= radius) ns)
  where
    (mpos, radius) = foldl (\mv@(_,mr) v@(_,r) -> if r > mr then v else mv) ((-1,-1,-1), -1) ns

dist :: Position -> Position -> Int
dist (x,y,z) (x',y',z') = (abs (x - x')) + (abs (y - y')) + (abs (z - z'))

main :: IO ()
main = do
  s <- readFile "input/day-23.input"
  let nonobots = parseInput s
  print $ numberOfMaxInRange nonobots
