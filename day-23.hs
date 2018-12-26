import System.Environment
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Heap as Heap

type Nanobot = (Position, Radius)
type Position = (Int, Int, Int)
type Radius = Int
type Box = (Position, Position)

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

nInRangeOfTheStrongest :: [Nanobot] -> Int
nInRangeOfTheStrongest ns = length (filter (\(pos, _) -> (dist mpos pos) <= radius) ns)
  where
    (mpos, radius) = foldl (\mv@(_,mr) v@(_,r) -> if r > mr then v else mv) ((-1,-1,-1), -1) ns

dist :: Position -> Position -> Int
dist (x,y,z) (x',y',z') = (abs (x - x')) + (abs (y - y')) + (abs (z - z'))

mapBounds :: [Nanobot] -> Box
mapBounds (((x,y,z),r):ns) = foldl extend ((x-r, y-r, z-r), (x+r,y+r, z+r)) ns
  where
    extend ((minx, miny, minz), (maxx, maxy, maxz)) ((x,y,z),r)
      = ((min minx (x-r), min miny (y-r), min minz (z-r)), (max maxx (x+r), max maxy (y+r), max maxz (z+r)))

inRegion :: Position -> Nanobot -> Bool
inRegion pos (npos, nr) = (dist pos npos) <= nr

boxDist :: Box -> Position -> Int
boxDist ((minx, miny, minz), (maxx, maxy, maxz)) (x,y,z)
  | x >= minx && x <= maxx && y >= miny && y <= maxy && z >= minz && z <= maxz = 0
  | otherwise = dist (x', y', z') (x,y,z)
    where
      x' = if (x >= minx && x <= maxx) then x else (if x < minx then minx else maxx)
      y' = if (y >= miny && y <= maxy) then y else (if y < miny then miny else maxy)
      z' = if (z >= minz && z <= maxz) then z else (if z < minz then minz else maxz)

intersectsBox :: Box -> Nanobot -> Bool
intersectsBox box (pos,r) = (boxDist box pos) <= r

isBoxAPoint :: Box -> Bool
isBoxAPoint (a, b) = a == b

divideBox :: Box -> (Box, Box)
divideBox box@((x,y,z), (x',y',z')) =
  let dx = x' - x
      dy = y' - y
      dz = z' - z
  in if dx >= dy && dx >= dz
    then divideBoxX box
    else if dy >= dx && dy >= dz
      then divideBoxY box
      else divideBoxZ box

divideBoxX :: Box -> (Box, Box)
divideBoxX (p0@(x,y,z), p1@(x',y',z')) = ((p0, (x+((x'-x) `div` 2),y',z')), ((x+((x'-x) `div` 2)+1,y,z), p1))

divideBoxY :: Box -> (Box, Box)
divideBoxY (p0@(x,y,z), p1@(x',y',z')) = ((p0, (x',y+((y'-y) `div` 2),z')), ((x,y+((y'-y) `div` 2)+1,z), p1))

divideBoxZ :: Box -> (Box, Box)
divideBoxZ (p0@(x,y,z), p1@(x',y',z')) = ((p0, (x',y',z+((z'-z) `div` 2))), ((x,y,z+((z'-z) `div` 2)+1), p1))

filterInboxWithSize :: Box -> [Nanobot] -> (Int, [Nanobot])
filterInboxWithSize box = foldl (\acc@(n,nbts) nbt -> if intersectsBox box nbt then (n+1, (nbt:nbts)) else acc) (0, [])

maxCoveragePosition :: [Nanobot] -> Maybe (Int, Position, [Nanobot])
maxCoveragePosition nbts = maxCoveragePosition' (Heap.singleton ((length nbts, mapBounds nbts), nbts))
  where
    maxCoveragePosition' :: Heap.MaxPrioHeap (Int, Box) [Nanobot] -> Maybe (Int, Position, [Nanobot])
    maxCoveragePosition' heap
      | Heap.null heap
          = Nothing
      | isBoxAPoint ((snd . fst) (fromJust (Heap.viewHead heap)))
          = let ((s, (a, b)), nbts) = fromJust (Heap.viewHead heap) in Just (s, a, nbts)
      | otherwise = maxCoveragePosition' (Heap.insert ((s0, b0), ns0) (Heap.insert ((s1, b1), ns1) (Heap.drop 1 heap)))
        where
          ((_, box), nbts) = fromJust (Heap.viewHead heap)
          (b0, b1) = divideBox box
          (s0, ns0) = filterInboxWithSize b0 nbts
          (s1, ns1) = filterInboxWithSize b1 nbts

main :: IO ()
main = do
  s <- readFile "input/day-23.input"
  let nanobots = parseInput s
  print $ nInRangeOfTheStrongest nanobots
  let (n, pos, _) = fromJust $ maxCoveragePosition nanobots
  print $ dist (0,0,0) pos
