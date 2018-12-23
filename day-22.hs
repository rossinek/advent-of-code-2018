import System.Environment
import Data.List.Split (splitOn)
import Control.Monad (forM_)
import Data.Array (Array, bounds, (!), elems, assocs, array)
import Data.Array.ST (runSTArray, STArray)
import Data.Array.MArray (newArray, writeArray, readArray)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.Heap as H

type CaveDepth = Int
type Position = (Int, Int)
type BlockType = Int
type Tool = Int

erosionLevels :: CaveDepth -> Position -> Position -> Position -> Array Position Int
erosionLevels depth (minX,minY) (maxX,maxY) (tx, ty) = canvasArr
  where
    canvasArr = runSTArray $ do
      arr <- newArray ((minX,minY), (maxX,maxY)) (-1)
      forM_ [minY..maxY] $ \y ->
        forM_ [minX..maxX] $ \x -> setErosionLevel arr (x,y)
      return arr
    setErosionLevel :: STArray s (Int, Int) Int -> (Int, Int) -> ST s ()
    setErosionLevel arr (x,y)
      | x==0 && y ==0 = do
        writeArray arr (0,0) (depth `mod` 20183)
      | x==0 = do
        writeArray arr (0,y) ((y * 48271 + depth) `mod` 20183)
      | y==0 = do
        writeArray arr (x,0) ((x * 16807 + depth) `mod` 20183)
      | x == tx && y == ty = do
        writeArray arr (x,y) (depth `mod` 20183)
      | otherwise = do
        p' <- readArray arr (x-1,y)
        p'' <- readArray arr (x,y-1)
        writeArray arr (x,y) (((p' * p'') + depth) `mod` 20183)

blockTypes :: Array Position Int -> Array Position BlockType
blockTypes arr = array (bounds arr) (map (\(pos, elev) -> (pos, elev `mod` 3)) (assocs arr))

riskLevel :: Array Position BlockType -> Int
riskLevel arr = sum (elems arr)

-- heap dijikstra
-- should be rewritten to allow btypes extending
minPath :: Array Position BlockType -> Position -> Maybe Int
minPath btypes target = minPath' (H.singleton (0, ((0, 0), 0))) Set.empty
  where
    siblings :: Set.Set (Position, Tool) -> (Position, Tool) -> [(Position, Tool)]
    siblings visited ((x,y), t)
      = filter (\v@((a,b),_) -> a>=0 && b>=0 && (allowedTool v) && (Set.notMember v visited)) sibs
          where
            sibs = [((x+1,y),t), ((x-1,y),t), ((x,y+1),t), ((x,y-1),t)]
    allowedTool :: (Position, Tool) -> Bool
    allowedTool (pos, tool)
      | isRock pos    = tool == 0 || tool == 1
      | isWet pos     = tool == 1 || tool == 2
      | isNarrow pos  = tool == 0 || tool == 2
    secondTool :: Position -> Tool -> Tool
    secondTool pos tool
      | isRock pos    = if tool == 0 then 1 else 0
      | isWet pos     = if tool == 1 then 2 else 1
      | isNarrow pos  = if tool == 0 then 2 else 0
    -- 0 or 1
    isRock :: Position -> Bool
    isRock pos = (btypes ! pos) == 0
    -- 1 or 2
    isWet :: Position -> Bool
    isWet pos = (btypes ! pos) == 1
    -- 0 or 2
    isNarrow :: Position -> Bool
    isNarrow pos = (btypes ! pos) == 2
    minPath' :: H.MinPrioHeap Int (Position, Tool) -> Set.Set (Position, Tool) -> Maybe Int
    minPath' heap visited
      | H.isEmpty heap = Nothing
      | otherwise = if hpos == target && htool == 0
        then Just hlen
        else if (hpos, htool) `Set.member` visited
          then minPath' htail visited
          else minPath' htail'' visited'
          where
            (Just hhead@(hlen, (hpos, htool))) = H.viewHead heap
            htail = H.drop 1 heap
            htail' = foldl (\h (pos,t) -> H.insert (hlen+1, (pos,t)) h) htail (siblings visited (hpos, htool))
            htail'' = H.insert (hlen+7, (hpos, secondTool hpos htool)) htail'
            visited' = Set.insert (hpos, htool) visited

main :: IO ()
main = do
  s <- readFile "input/day-22.input"
  let (cd':tr':_) = lines s
  let caveDepth = read (drop 7 cd') :: Int
  let (x:y:_) = map read (splitOn "," (drop 8 tr')) :: [Int]
  let target = (x,y)
  let btypes = blockTypes $ erosionLevels caveDepth (0,0) (x,y) target
  print $ btypes ! target
  print $ riskLevel btypes
  -- check minPath comment to understand magic (x+y,y*2) bounds
  let btypes' = blockTypes $ erosionLevels caveDepth (0,0) (x+y,y*2) target
  print $ minPath btypes' target

