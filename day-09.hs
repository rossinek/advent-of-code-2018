import System.Environment
import Helpers.Circle


type Gameplay = Players -> Turn
type Players = Int
type Turn = (MarblesCircle, ScoresCircle)
type ScoresCircle = Circle Score
type Score = Int
type Marble = Int
type MarblesCircle = Circle Marble

parseInput :: String -> (Players, Marble)
parseInput s = (players, lastMarble)
  where
    players = read ((words s) !! 0)
    lastMarble = read ((words s) !! 6)

gameplayFor :: Players -> Gameplay
gameplayFor nplayers = gameplay
  where
    gameplay :: Gameplay
    gameplay n  | n == 0            = (fromList [0], fromList [0 | p <- [1..nplayers]])
                | (n `mod` 23) == 0 = specialMove n (gameplay (n-1))
                | otherwise         = standardMove n (gameplay (n-1))

    standardMove :: Marble -> Turn -> Turn
    standardMove n (marbles, scores) = (insertAfter n (rotateRight marbles), rotateRight scores)

    specialMove :: Marble -> Turn -> Turn
    specialMove n (marbles, scores) = (newMarbles, rotateRight (update newScore scores))
      where
        rotatedMarbles = rotateNLeft 7 marbles
        removedMarble = current rotatedMarbles
        newMarbles = removeAfter rotatedMarbles
        newScore = (current scores) + removedMarble + n

highscore :: Players -> Marble -> Score
highscore players lastMarble = maximum (toList scores)
  where
    scores = (snd (gameplayFor players lastMarble))

main :: IO ()
main = do
  s <- readFile "input/day-09.input"
  let (players, lastMarble) = parseInput s
  print $ highscore players lastMarble
  print $ highscore players (lastMarble * 100)
