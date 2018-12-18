import System.Environment
import Control.Monad (mapM_, forM_)
import Data.Array (Array, bounds, (!), elems)
import Data.Array.ST (runSTArray)
import Data.Array.MArray (newArray, writeArray)

type Type = Char
type LumberCollection = Array Position Type
type Position = (Int, Int)

parseInput :: String -> LumberCollection
parseInput s = canvasArr
  where
    ls = lines s
    width = length (head ls)
    height = length ls
    canvasArr = runSTArray $ do
      arr <- newArray ((0, 0), (width-1, height-1)) '.'
      forM_ [0..(height-1)] $ \y ->
        forM_ [0..(width-1)] $ \x -> do
          writeArray arr (x,y) ((ls !! y) !! x)
      return arr

mapToStrings :: LumberCollection -> [String]
mapToStrings tm = map strLine yCords
  where
    (_, (w, h)) = bounds tm
    xCords = enumFromThenTo 0 1 w
    yCords = enumFromThenTo 0 1 h
    strLine :: Int -> String
    strLine y = map (\x -> tm ! (x, y)) xCords

drawCanvas :: LumberCollection -> IO ()
drawCanvas tm = mapM_ putStrLn $ mapToStrings tm

nextTick :: LumberCollection -> LumberCollection
nextTick area = canvasArr
  where
    bb@(_, (w, h)) = bounds area
    nextTick' :: Position -> Type -> Type
    nextTick' pos '.' = if atLeast3 '|' (adjacent area pos) then '|' else '.'
    nextTick' pos '|' = if atLeast3 '#' (adjacent area pos) then '#' else '|'
    nextTick' pos '#' = if (atLeast1 '#' (adjacent area pos)) && (atLeast1 '|' (adjacent area pos)) then '#' else '.'
    canvasArr = runSTArray $ do
      arr <- newArray bb '.'
      forM_ [0..h] $ \y ->
        forM_ [0..w] $ \x -> do
          writeArray arr (x,y) (nextTick' (x,y) (area ! (x,y)))
      return arr

nextNTicks :: Int -> LumberCollection -> LumberCollection
nextNTicks 0 area = area
nextNTicks n area = nextNTicks (n-1) (nextTick area)

numOfType :: Type -> LumberCollection -> Int
numOfType t area = count t (elems area)
  where
    count :: Eq a => a -> [a] -> Int
    count a [] = 0
    count a (x:as) | x == a     = 1 + (count a as)
                   | otherwise  = count a as

resourceValue :: LumberCollection -> Int
resourceValue area = (numOfType '#' area) * (numOfType '|' area)

atLeast3 :: Type -> [Type] -> Bool
atLeast3 x [] = False
atLeast3 x (t:ts) = if t == x then atLeast2 x ts else atLeast3 x ts

atLeast2 :: Type -> [Type] -> Bool
atLeast2 x [] = False
atLeast2 x (t:ts) = if t == x then atLeast1 x ts else atLeast2 x ts

atLeast1 :: Type -> [Type] -> Bool
atLeast1 x [] = False
atLeast1 x (t:ts) = if t == x then True else atLeast1 x ts

adjacent :: LumberCollection -> Position -> [Type]
adjacent area (x,y) = map (area !) poss
  where
    (_, (w, h)) = bounds area
    poss' = [(x,y-1), (x+1,y-1), (x+1,y), (x+1,y+1), (x,y+1), (x-1,y+1), (x-1,y), (x-1,y-1)]
    poss = filter (\(x',y') -> x' >= 0 && x' <= w && y' >= 0 && y' <= h) poss'

main :: IO ()
main = do
  s <- readFile "input/day-18.input"
  let area = parseInput s
  print $ resourceValue (nextNTicks 10 area)
