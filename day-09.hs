import System.Environment

type Gameplay = Int -> (Marbles, Scores)
type Player = Int
type Score = Int
type Scores = [(Player, Score)]
type Marbles = [Int]

parseInput :: String -> (Int, Int)
parseInput s = (players, lastMarble)
  where
    players = read ((words s) !! 0)
    lastMarble = read ((words s) !! 6)

gameplayFor :: Int -> Gameplay
gameplayFor nplayers = gameplay
  where
    gameplay :: Gameplay
    gameplay n  | n == 0            = ([0], [(p, 0) | p <- [1..nplayers]])
                | (n `mod` 23) == 0 = specialMove n (gameplay (n-1))
                | otherwise         = standardMove n (gameplay (n-1))

    standardMove :: Int -> (Marbles, Scores) -> (Marbles, Scores)
    standardMove n (board, (ps:pss)) = (next board, pss ++ [ps])
      where
        next :: Marbles -> Marbles
        next (x:y:tail) = (n:tail) ++ [x, y]
        next xs = (n:xs)

    specialMove :: Int -> (Marbles, Scores) -> (Marbles, Scores)
    specialMove n (board, ((p, s):pss)) = (nextBoard, pss ++ [(p, newScore)])
      where
        (m:nextBoard) = next board
        newScore = s + n + m

        next :: Marbles -> Marbles
        next board = b2 ++ b1
          where (b1, b2) = splitAt (length board - 7) board

highscore :: Int -> Int -> Score
highscore players lastMarble = maximum (map snd scores)
  where
    scores = (snd (gameplayFor players lastMarble))

main :: IO ()
main = do
  s <- readFile "input/day-09.input"
  let (players, lastMarble) = parseInput s
  print $ highscore players lastMarble
  -- print $ highscore players (lastMarble * 100)
