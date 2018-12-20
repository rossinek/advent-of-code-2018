import System.Environment
import Data.Array (Array, array, bounds, (!))
import Control.Monad (mapM_)
import Data.List.Extra (minimumOn)
import Data.Maybe
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Heap as Heap

type Position = (Int, Int)
type HitPoints = Int
type UnitType = Char

type WallsMap = Array Position Bool
type UnitsMap = Map.Map Position Unit
type Graves = Set.Set Position

type Unit = (UnitType, HitPoints)

initialHP :: Int
initialHP = 200

parseInput :: String -> (WallsMap, UnitsMap)
parseInput s = (array ((0, 0), (height-1, width-1)) values, units)
  where
    ls = lines s
    width = length (head ls)
    height = length ls
    values' = [((y,x), (ls !! y) !! x) | x <- [0..(width-1)], y <- [0..(height-1)]]
    (values, units) = foldl separateMaps ([], Map.empty) values'
    separateMaps :: ([(Position, Bool)], UnitsMap) -> (Position, Char) -> ([(Position, Bool)], UnitsMap)
    separateMaps (values, units) (pos, field)
      | field == '.'                 = (((pos, False):values), units)
      | field == '#'                 = (((pos, True):values), units)
      | field == 'G' || field == 'E' = (((pos, False):values), Map.insert pos (field, initialHP) units)

mapsToStrings :: WallsMap -> UnitsMap -> [String]
mapsToStrings wm um = map (\y -> (strLine y) ++ "  " ++ (show (lineUnits y))) yCords
  where
    (_, (h, w)) = bounds wm
    xCords = enumFromThenTo 0 1 w
    yCords = enumFromThenTo 0 1 h
    strLine :: Int -> String
    strLine y = map (\x -> Map.findWithDefault (wallChar (wm ! (y, x))) (y,x) (Map.map fst um)) xCords
    lineUnits :: Int -> [Unit]
    lineUnits y = Map.elems (Map.filterWithKey (\(y',_) _ -> y' == y) um)
    wallChar :: Bool -> Char
    wallChar True = '#'
    wallChar False = '.'

drawMaps :: WallsMap -> UnitsMap -> IO ()
drawMaps wm um = mapM_ putStrLn $ mapsToStrings wm um

nextRound :: WallsMap -> UnitsMap -> (UnitsMap, Graves)
nextRound walls unitsMap = foldl turn (unitsMap, Set.empty) unitsQueue
  where
    unitsQueue = Map.toAscList unitsMap
    turn :: (UnitsMap, Graves) -> (Position, Unit) -> (UnitsMap, Graves)
    turn (units, graves) (pos, unit)
      | pos `Set.member` graves = (units, graves)
      | otherwise               = uncurry attack (move (pos, unit) (units, graves))
    move :: (Position, Unit) -> (UnitsMap, Graves) -> ((Position, Unit), (UnitsMap, Graves))
    move (pos, unit) (umap, graves)
      | (not . null) (targetsInRegion walls umap (pos, unit)) = ((pos, unit), (umap, graves))
      | otherwise = if isNothing fstep
        then ((pos, unit), (umap, graves))
        else ((fromJust fstep, unit), (Map.insert (fromJust fstep) unit (Map.delete pos umap), graves))
          where
            fstep = firstStep walls umap (pos, unit)
    attack :: (Position, Unit) -> ((UnitsMap, Graves)) -> (UnitsMap, Graves)
    attack (pos, unit) (umap, graves) = if null targets
      then (umap, graves)
      else (umap', graves')
        where
          targets = targetsInRegion walls umap (pos, unit)
          (weakestPos, (weakestType, weakestHp)) = minimumOn (snd . snd) targets
          weakestHp' = weakestHp - 3
          isKilled = weakestHp' <= 0
          umap' = if isKilled
            then Map.delete weakestPos umap
            else Map.insert weakestPos (weakestType, weakestHp') umap
          graves' = if isKilled then Set.insert weakestPos graves else graves

combatFinished :: UnitsMap -> Bool
combatFinished umap = let ((ut,_):uts) = Map.elems umap in not (hasTarget ut uts)

hasTarget :: UnitType -> [Unit] -> Bool
hasTarget utype [] = False
hasTarget utype ((ut,_):uts) = utype /= ut || hasTarget utype uts

combat :: WallsMap -> UnitsMap -> (Int, Int, UnitsMap)
combat walls unitsMap = combat' walls unitsMap (-1)

combat' :: WallsMap -> UnitsMap -> Int -> (Int, Int, UnitsMap)
combat' walls unitsMap n = if combatFinished unitsMap
  then (n, sum (map snd (Map.elems unitsMap)), unitsMap)
  else combat' walls (fst (nextRound walls unitsMap)) (n+1)

firstStep :: WallsMap -> UnitsMap -> (Position, Unit) -> Maybe Position
firstStep walls umap (initPos, (unitType, hp)) = firstStep' (Heap.singleton (0, initPos, initPos)) Map.empty
  where
    firstStep' :: Heap.MinHeap (Int, Position, Position) -> Map.Map Position Position -> Maybe Position
    firstStep' heap visited
      | Heap.isEmpty heap = Nothing
      | otherwise = if hpos `Map.member` visited
        then firstStep' htail visited
        else if isUnit && hpos /= initPos
          then if isTargetUnit
            then Just (pathFirstStep (hpos, hparent) visited)
            else firstStep' htail visited'
          else firstStep' htail' visited'
            where
              (plen, hpos, hparent) = fromJust (Heap.viewHead heap)
              maybeUnit = (umap Map.!? hpos)
              isUnit = isJust maybeUnit
              isTargetUnit = isUnit && (fst (fromJust maybeUnit) /= unitType)
              visited' = Map.insert hpos hparent visited
              htail = Heap.drop 1 heap
              newPools = neighborhood walls hpos
              htail' = foldl (\h pos -> Heap.insert (plen+1, pos, hpos) h) htail (neighborhood walls hpos)

pathFirstStep :: (Position, Position) -> Map.Map Position Position -> Position
pathFirstStep (pos, parent) visited = if parent == nextParent
  then pos
  else pathFirstStep (parent, nextParent) visited
    where
      nextParent = visited Map.! parent

targetsInRegion :: WallsMap -> UnitsMap -> (Position, Unit) -> [(Position, Unit)]
targetsInRegion walls umap (pos, (ut,_)) = filter (\(_,(ut',_)) -> ut' /= ut) (foldl maybeShiftWithPos [] (neighborhood walls pos))
  where
    maybeShiftWithPos :: [(Position, Unit)] -> Position -> [(Position, Unit)]
    maybeShiftWithPos acc pos = maybeShift (maybeWithPos pos (umap Map.!? pos)) acc

neighborhood :: WallsMap -> Position -> [Position]
neighborhood walls (y,x) = filter notWall (filter inBounds [(y-1,x), (y,x-1), (y,x+1), (y+1,x)])
  where
    ((miny,minx), (maxy,maxx)) = bounds walls
    inBounds :: Position -> Bool
    inBounds (y,x) = y >= miny && y <= maxy && x >= minx && x <= maxx
    notWall :: Position -> Bool
    notWall pos = not (walls ! pos)

maybeShift :: Maybe a -> [a] -> [a]
maybeShift Nothing x = x
maybeShift (Just a) x = (a:x)

maybeWithPos :: Position -> Maybe a -> Maybe (Position, a)
maybeWithPos _ Nothing = Nothing
maybeWithPos pos (Just a) = Just (pos, a)

main :: IO ()
main = do
  s <- readFile "input/day-15.input"
  let (wallMap, unitsMap) = parseInput s
  -- drawMaps wallMap unitsMap
  let (n,hps,finished) = combat wallMap unitsMap
  -- drawMaps wallMap finished
  print (n * hps)

