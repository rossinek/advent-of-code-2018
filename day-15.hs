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
type ElfAttackPower = Int

data RoundState = Round UnitsMap Graves | Finished UnitsMap Graves deriving Show

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

nextRound :: ElfAttackPower -> WallsMap -> UnitsMap -> RoundState
nextRound elfAttack walls unitsMap = foldl turn (Round unitsMap Set.empty) unitsQueue
  where
    unitsQueue = Map.toAscList unitsMap
    turn :: RoundState -> (Position, Unit) -> RoundState
    turn rs@(Finished units graves) (pos, unit) = rs
    turn roundstate@(Round _ graves) (pos, unit)
      | pos `Set.member` graves = roundstate
      | otherwise               = uncurry attack (uncurry move (updateRoundState (pos, unit) roundstate))
    updateRoundState :: (Position, Unit) -> RoundState -> ((Position, Unit), RoundState)
    updateRoundState us rs@(Finished _ _) = (us,rs)
    updateRoundState us@(pos, _) roundstate@(Round umap graves) = if combatFinished roundstate
      then (us, Finished umap graves)
      else ((pos, umap Map.! pos), roundstate)
    move :: (Position, Unit) -> RoundState -> ((Position, Unit), RoundState)
    move (pos, unit) rs@(Finished _ _) = ((pos, unit), rs)
    move (pos, unit@(ut,_)) roundstate@(Round umap graves)
      | anyTargetInRegion walls umap ut pos = ((pos, unit), roundstate)
      | otherwise = if isNothing fstep
        then ((pos, unit), roundstate)
        else ((fromJust fstep, unit), Round (Map.insert (fromJust fstep) unit (Map.delete pos umap)) graves)
          where
            fstep = firstStep walls umap (pos, unit)
    attack :: (Position, Unit) -> RoundState -> RoundState
    attack (pos, unit) rs@(Finished _ _) = rs
    attack (pos, unit) roundstate@(Round umap graves) = if null targets
      then roundstate
      else Round umap' graves'
        where
          targets = targetsInRegion walls umap (pos, unit)
          (weakestPos, (weakestType, weakestHp)) = minimumOn (\(pp, (_,php)) -> (php, pp)) targets
          weakestHp' = weakestHp - (if weakestType == 'G' then elfAttack else 3)
          isKilled = weakestHp' < 1
          umap' = if isKilled
            then Map.delete weakestPos umap
            else Map.insert weakestPos (weakestType, weakestHp') umap
          graves' = if isKilled then Set.insert weakestPos graves else graves

combatFinished :: RoundState -> Bool
combatFinished (Round umap _) = let ((ut,_):uts) = Map.elems umap in not (hasTarget ut uts)

hasTarget :: UnitType -> [Unit] -> Bool
hasTarget utype [] = False
hasTarget utype ((ut,_):uts) = utype /= ut || hasTarget utype uts

combat :: WallsMap -> UnitsMap -> (Int, Int, UnitsMap)
combat walls unitsMap = combat' walls unitsMap 0

combat' :: WallsMap -> UnitsMap -> Int -> (Int, Int, UnitsMap)
combat' walls unitsMap n = if roundFinished nextR
  then (n, sum (map snd (Map.elems (roundUnits nextR))), roundUnits nextR)
  else combat' walls (roundUnits nextR) (n+1)
    where
      nextR = nextRound 3 walls unitsMap

roundUnits :: RoundState -> UnitsMap
roundUnits (Round unitsMap _) = unitsMap
roundUnits (Finished unitsMap _) = unitsMap

roundGraves :: RoundState -> Graves
roundGraves (Round _ graves) = graves
roundGraves (Finished _ graves) = graves

roundFinished :: RoundState -> Bool
roundFinished (Finished _ _) = True
roundFinished _ = False

minLosslessCombat :: WallsMap -> UnitsMap -> (Int, Int, Int, UnitsMap)
minLosslessCombat walls initialUnitsMap = minLosslessCombat' initialUnitsMap 4 0
  where
    minLosslessCombat' :: UnitsMap -> ElfAttackPower -> Int -> (Int, Int, Int, UnitsMap)
    minLosslessCombat' unitsMap elfAttack n = if elfDied graves unitsMap
      then minLosslessCombat' initialUnitsMap (elfAttack+1) 0
      else if roundFinished nround
        then (elfAttack, n, sum (map snd (Map.elems unitsMap')), unitsMap')
        else minLosslessCombat' unitsMap' elfAttack (n+1)
          where
            nround = nextRound elfAttack walls unitsMap
            unitsMap' = roundUnits nround
            graves = roundGraves nround

elfDied :: Graves -> UnitsMap -> Bool
elfDied graves unitsMap = (not . Set.null) $ Set.filter (\pos -> (fst (unitsMap Map.! pos)) == 'E') graves

firstStep :: WallsMap -> UnitsMap -> (Position, Unit) -> Maybe Position
firstStep walls umap (initPos, (unitType, _)) = if isNothing cenem
  then Nothing
  else minClosestPos
  where
    cenem = closestTargetSibling (Heap.singleton (0,initPos)) Set.empty (anyTargetInRegion walls umap unitType)
    minClosestPos = closestTargetSibling (Heap.singleton (0, fromJust cenem)) Set.empty (isNeighbor initPos)

    closestTargetSibling :: Heap.MinHeap (Int, Position) -> Set.Set Position -> (Position -> Bool) -> Maybe Position
    closestTargetSibling heap visited isTargetSibling
      | Heap.isEmpty heap = Nothing
      | otherwise = if hpos `Set.member` visited
        then closestTargetSibling htail visited isTargetSibling
        else if hpos /= initPos && isUnit
          then closestTargetSibling htail visited' isTargetSibling
          else if isTargetSibling hpos
            then Just hpos
            else closestTargetSibling htail' visited' isTargetSibling
        where
          (plen, hpos) = fromJust (Heap.viewHead heap)
          maybeUnit = (umap Map.!? hpos)
          isUnit = isJust maybeUnit
          visited' = Set.insert hpos visited
          htail = Heap.drop 1 heap
          newPools = neighborhood walls hpos
          htail' = foldl (\h pos -> Heap.insert (plen+1, pos) h) htail (neighborhood walls hpos)

anyTargetInRegion :: WallsMap -> UnitsMap -> UnitType -> Position -> Bool
anyTargetInRegion walls umap unitType pos = anyTargetInRegion' (neighborhood walls pos)
  where
    anyTargetInRegion' :: [Position] -> Bool
    anyTargetInRegion' [] = False
    anyTargetInRegion' (pos:pss) = let maybeUnit = (umap Map.!? pos) in if isNothing maybeUnit
      then anyTargetInRegion' pss
      else (fst (fromJust maybeUnit) /= unitType) || anyTargetInRegion' pss

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

isNeighbor :: Position -> Position -> Bool
isNeighbor (y,x) (y',x') = ((abs (y - y')) + (abs (x - x'))) == 1

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
  let (elfAttack, n',hps',finished') = minLosslessCombat wallMap unitsMap
  -- drawMaps wallMap finished'
  print (n' * hps')
