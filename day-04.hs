import System.Environment
import Data.List.Split (splitOn)
import Data.List (sortOn)
import qualified Data.Map.Lazy as Map

type Date = String
type Time = Int
type Nap = (Time, Time)

type Guard = Int
type NapsMap = Map.Map Guard [Nap]

data Event = Shift Guard | NapStart Time | NapEnd Time deriving Show

parseInput :: String -> [Event]
parseInput s = map snd (sortOn fst (map parseLog (lines s)))
  where
    parseLog :: String -> (Date, Event)
    parseLog line = (date, event)
      where
        (d:v:_) = splitOn "] " line
        date = tail d
        event = toEvent v

        toEvent :: String -> Event
        toEvent ('f':_) = NapStart (getTime date)
        toEvent ('w':_) = NapEnd (getTime date)
        toEvent s = Shift (read (takeWhile (/= ' ') (drop 7 s)) :: Int)

getTime :: Date -> Time
getTime d = read (drop 14 d) :: Int

napsMap :: [Event] -> NapsMap
napsMap events = napsMap' Map.empty events
  where
    upsert :: [Nap] -> Maybe [Nap] -> Maybe [Nap]
    upsert naps Nothing = Just naps
    upsert naps (Just ns) = Just (naps ++ ns)

    consumeNaps :: [Nap] -> [Event] -> ([Nap], [Event])
    consumeNaps naps ((NapStart sd):(NapEnd ed):evts) = consumeNaps ((sd, ed):naps) evts
    consumeNaps naps evts = (naps, evts)

    napsMap' :: NapsMap -> [Event] -> NapsMap
    napsMap' nmap [] = nmap
    napsMap' nmap ((Shift g):evts) = napsMap' (Map.alter (upsert naps) g nmap) rest
      where
        (naps, rest) = consumeNaps [] evts

duration :: Nap -> Int
duration (b, e) = e - b

sleepyGuard :: NapsMap -> Guard
sleepyGuard nmap = fst $ Map.foldrWithKey moreSleepy (0, 0) nmap
  where
    moreSleepy :: Guard -> [Nap] -> (Guard, Int) -> (Guard, Int)
    moreSleepy g naps acc@(mg, mv) = let total = sum (map duration naps)
      in if (total > mv) then (g, total) else acc


sleepyMinute :: [Nap] -> (Time, Int)
sleepyMinute naps = (Map.foldrWithKey isGreater (0, 0) (minuteMap Map.empty naps))
  where
    isGreater :: Time -> Int -> (Time, Int) -> (Time, Int)
    isGreater m v acc@(pm, pv) = if v > pv then (m, v) else acc

    incr :: Maybe Int -> Maybe Int
    incr Nothing = Just 1
    incr (Just n) = Just (n+1)

    minuteMap :: Map.Map Time Int -> [Nap] -> Map.Map Time Int
    minuteMap tmap [] = tmap
    minuteMap tmap ((b,e):xs) = minuteMap updatedMap xs
      where
        updatedMap = foldl (\acc t -> Map.alter incr t acc) tmap [b..(e-1)]

strategy1 :: NapsMap -> Int
strategy1 nmap = let sleepy = sleepyGuard nmap in sleepy * (fst (sleepyMinute (nmap Map.! sleepy)))

strategy2 :: NapsMap -> Int
strategy2 nmap = guard * minute
  where
    mmap = (Map.map sleepyMinute nmap)
    (guard, (minute, _)) = (Map.foldrWithKey isGreater (0, (0, 0)) mmap)
    isGreater :: Guard -> (Time, Int) -> (Guard, (Time, Int)) -> (Guard, (Time, Int))
    isGreater g (m, v) acc@(pg, (pm, pv)) = if v > pv then (g, (m, v)) else acc

main :: IO ()
main = do
  s <- readFile "input/day-04.input"
  let events = parseInput s
  let nmap = napsMap events
  print $ strategy1 nmap
  print $ strategy2 nmap
