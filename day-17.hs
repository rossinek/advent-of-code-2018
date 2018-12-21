import System.Environment
import Control.Monad (forM_)
import Data.Array (Array, bounds, (!))
import Data.Array.ST (runSTArray)
import Data.Array.MArray (newArray, writeArray)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

type ClayMap = Array Position Bool
type Position = (Int, Int)
type ClayLine = (Direction, Position, Int)
data Direction = Vertical | Horizontal deriving Eq
type WaterHead = Set.Set Position
type StableWater = Set.Set Position

parseInput :: String -> ClayMap
parseInput s = clayMap
  where
    cLines = map parseClayLine (lines s)
    clayMap = runSTArray $ do
      arr <- newArray (linesToBounds cLines) False
      forM_ (foldl (\acc l -> (lineToPoints l) ++ acc) [] cLines) (\p -> writeArray arr p True)
      return arr

parseClayLine :: String -> ClayLine
parseClayLine s = (direction, position, rVal1 - rVal0)
  where
    (ls:rs:_) = splitOn ", " s
    direction = if (head ls) == 'x' then Vertical else Horizontal
    lVal = read (drop 2 ls) :: Int
    (rVal0:rVal1:_) = map read (splitOn ".." (drop 2 rs))
    position = if direction == Vertical then (lVal, rVal0) else (rVal0, lVal)

lineToPoints :: ClayLine -> [Position]
lineToPoints (Vertical, (x,y), len) = [ (x,y') | y' <- [y..y+len] ]
lineToPoints (Horizontal, (x,y), len) = [ (x',y) | x' <- [x..x+len] ]

linesToBounds :: [ClayLine] -> (Position, Position)
linesToBounds cls@((_, pos, _):_) = ((mmx-1,mmy), (mxx+1,mxy))
    where
      ((mmx,mmy), (mxx,mxy)) = foldl maxBB (pos, pos) cls
      maxBB :: (Position, Position) -> ClayLine -> (Position, Position)
      maxBB ((minx,miny), (maxx,maxy)) line@(d, (x,y), len) = ((minx',miny'), (maxx',maxy'))
        where
          minx' = min minx x
          miny' = min miny y
          (x', y') = lineEnd line
          maxx' = max maxx x'
          maxy' = max maxy y'

lineEnd :: ClayLine -> Position
lineEnd (Vertical, (x,y), l) = (x, y+l)
lineEnd (Horizontal, (x,y), l) = (x+l, y)

drawClayMap :: ClayMap -> StableWater -> WaterHead -> IO ()
drawClayMap clayMap sw wh = mapM_ putStrLn strings
  where
    ((minX, minY), (maxX, maxY)) = bounds clayMap
    xCords = enumFromThenTo minX (minX + 1) maxX
    yCords = enumFromThenTo minY (minY + 1) maxY
    strings = map strLine yCords
    toChar :: Position -> Char
    toChar pos = if clayMap ! pos
      then '#'
      else if pos `Set.member` sw
        then '~'
        else if pos `Set.member` wh
          then '|'
          else '.'
    strLine :: Int -> String
    strLine y = map (\x -> toChar $ (x, y)) xCords

fillClayMap :: ClayMap -> (StableWater, WaterHead)
fillClayMap clayMap = fillClayMap' clayMap (Set.empty, Set.singleton (500,minY))
  where
    ((minX, minY), (maxX, maxY)) = bounds clayMap

fillClayMap' :: ClayMap -> (StableWater, WaterHead) -> (StableWater, WaterHead)
fillClayMap' clayMap (stable, whead) = if finished then (newStable, newWhead) else fillClayMap' clayMap (newStable, newWhead)
    where
      ((minX, minY), (maxX, maxY)) = bounds clayMap
      newWHead'' = Set.foldl (nextTick stable) Set.empty whead
      newWHead' = Set.union whead newWHead''
      (newStable', newWhead) = Set.partition (isStable stable newWHead') newWHead'
      newStable = Set.union stable newStable'
      finished = (Set.size newStable' == 0) && (Set.size (Set.difference newWHead'' whead) == 0)

      nextTick :: StableWater -> WaterHead -> Position -> WaterHead
      nextTick sw whead (x,y) = nextWHead
        where
          whead' = if isWall (x-1,y)  then whead else Set.insert (x-1,y) whead
          whead'' = if isWall (x+1,y) then whead' else Set.insert (x+1,y) whead'
          nextWHead = if isFilled sw whead (x,y+1)
            then whead''
            else if inBoundsY (x,y+1) then Set.insert (x,y+1) whead else whead
      isStable :: StableWater -> WaterHead -> Position -> Bool
      isStable sw wh (x,y) = (isFilled sw wh (x,y+1))
          && ((isFilled sw wh (x-1,y)) || (isConnectingWalls sw wh (x-1,y)))
          && ((isFilled sw wh (x+1,y)) || (isConnectingWalls sw wh (x+1,y)))
      isFilled :: StableWater -> WaterHead -> Position -> Bool
      isFilled sw wh pos = (isWall pos) || (pos `Set.member` sw)
      inBoundsY :: Position -> Bool
      inBoundsY (_,y) = y >= minY && y <= maxY
      inBounds :: Position -> Bool
      inBounds (x,y) = x >= minX && x <= maxX && y >= minY && y <= maxY
      isWall :: Position -> Bool
      isWall pos = if inBounds pos then clayMap ! pos else False
      isConnectingWalls :: StableWater -> WaterHead -> Position-> Bool
      isConnectingWalls sw wh (x,y) = isConnRight (x+1,y) && isConnLeft (x-1,y)
        where
          isConnLeft pos@(x,y) = if isFilled sw wh (x,y)
            then True
            else ((x,y) `Set.member` wh) && (isConnLeft (x-1, y))
          isConnRight pos@(x,y) = if isFilled sw wh (x,y)
            then True
            else ((x,y) `Set.member` wh) && (isConnRight (x+1, y))

main :: IO ()
main = do
  s <- readFile "input/day-17.input"
  let clayMap = parseInput s
  let (sw, wh) = fillClayMap clayMap
  -- drawClayMap clayMap sw wh
  print $ Set.size sw + Set.size wh
  print $ Set.size sw
