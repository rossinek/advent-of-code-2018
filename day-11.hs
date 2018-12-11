import System.Environment
import Data.List.Extras.Argmax (argmaxWithMax)

type Cell = (Int, Int)
type Size = Int
type GridSerial = Int
type Box = (Cell, Size)

hundredsDigit :: Int -> Int
hundredsDigit n = (n `div` 100) `mod` 10

powerLevel :: GridSerial -> Cell -> Int
powerLevel serial (x,y) = (hundredsDigit (((rackId * y) + serial) * rackId))  - 5
  where
    rackId = x + 10
    part = ((rackId * y) + serial) * rackId

boxPowerLevel :: GridSerial -> Box -> Int
boxPowerLevel serial ((x,y), s) = sum levels
  where
    levels = do
      x' <- [x..(x+s-1)]
      y' <- [y..(y+s-1)]
      return $ powerLevel serial (x', y')

maxCellWithValue :: GridSerial -> Size -> (Cell, Int)
maxCellWithValue serial s = argmaxWithMax (\c -> boxPowerLevel serial (c,s)) cells
  where cells = [(x,y) | x <- [1..(300-s+1)], y <- [1..(300-s+1)]]

maxCell :: GridSerial -> Size -> Cell
maxCell serial s = let (cell, _) = maxCellWithValue serial s in cell

main :: IO ()
main = do
  s <- readFile "input/day-11.input"
  let serial = read s :: Int
  print $ maxCell serial 3

