import System.Environment
import Data.CircularList
import Data.Maybe (fromJust)

parseInput :: String -> (Int, Int)
parseInput s = (players, lastMarble)
  where
    players = read ((words s) !! 0)
    lastMarble = read ((words s) !! 6)

type Gameplay = Int -> Turn
type Turn = (Marbles, Scores)
type Scores = CList Int
type Marbles = CList Int

gameplayFor :: Int -> Gameplay
gameplayFor nplayers = gameplay
  where
    gameplay :: Gameplay
    gameplay n  | n == 0            = (fromList [0], fromList [0 | p <- [1..nplayers]])
                | (n `mod` 23) == 0 = specialMove n (gameplay (n-1))
                | otherwise         = standardMove n (gameplay (n-1))

    standardMove :: Int -> Turn -> Turn
    standardMove n (marbles, scores) = (insertL n (rotR marbles), rotR scores)

    specialMove :: Int -> Turn -> Turn
    specialMove n (marbles, scores) = (newMarbles, rotR (update newScore scores))
      where
        rotatedMarbles = rotNL 7 marbles
        removedMarble = fromJust (focus rotatedMarbles)
        newMarbles = removeR rotatedMarbles
        newScore = (fromJust (focus scores)) + removedMarble + n

highscore :: Int -> Int -> Int
highscore players lastMarble = maximum (toList scores)
  where
    scores = (snd (gameplayFor players lastMarble))

main :: IO ()
main = do
  s <- readFile "input/day-09.input"
  let (players, lastMarble) = parseInput s
  print $ highscore players lastMarble
  print $ highscore players (lastMarble * 100)
