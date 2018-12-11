import System.Environment
import Data.List.Extras.Argmax (argmaxWithMax)
import qualified Data.Map.Lazy as Map

type Cell = (Int, Int)
type Scale = Int
type GridSerial = Int
type Box = (Cell, Scale)
type CellMap = (Map.Map Cell PowerLevel)
type PowerLevel = Int

bSIZE = 300
mbSIZE = 300

hundredsDigit :: Int -> Int
hundredsDigit n = (n `div` 100) `mod` 10

powerLevel :: GridSerial -> Cell -> PowerLevel
powerLevel serial (x,y) = (hundredsDigit (((rackId * y) + serial) * rackId))  - 5
  where
    rackId = x + 10
    part = ((rackId * y) + serial) * rackId

boxPowerLevel :: GridSerial -> Box -> PowerLevel
boxPowerLevel serial ((x,y), s) = sum levels
  where
    levels = do
      x' <- [x..(x+s-1)]
      y' <- [y..(y+s-1)]
      return $ powerLevel serial (x', y')

maxCellWithValue :: GridSerial -> Scale -> (Cell, PowerLevel)
maxCellWithValue serial s = argmaxWithMax (\c -> boxPowerLevel serial (c,s)) cells
  where cells = [(x,y) | x <- [1..(bSIZE-s+1)], y <- [1..(bSIZE-s+1)]]

maxCell :: GridSerial -> Scale -> Cell
maxCell serial s = let (cell, _) = maxCellWithValue serial s in cell

maxBox :: GridSerial -> Box
maxBox serial = fst $ maxBoxWithValue serial

emptyValueMap :: CellMap
emptyValueMap = Map.fromList [((x,y), 0) | x <- [1..bSIZE+1], y <- [1..bSIZE+1]]

cellValueMap :: GridSerial -> CellMap
cellValueMap serial = foldl addCellValue Map.empty cells
  where
    cells = [(x,y) | x <- [1..bSIZE], y <- [1..bSIZE]]
    addCellValue :: CellMap -> Cell -> CellMap
    addCellValue cellMap cell = Map.insert cell (powerLevel serial cell) cellMap

maxInMap :: CellMap -> (Cell, PowerLevel)
maxInMap cellMap = Map.foldrWithKey betterCell ((0,0),0) cellMap
  where
    betterCell :: Cell -> PowerLevel -> (Cell, PowerLevel) -> (Cell, PowerLevel)
    betterCell cell val (mCell, mVal) = if val > mVal
      then (cell, val)
      else (mCell, mVal)

maxBoxWithValue :: GridSerial -> (Box, PowerLevel)
maxBoxWithValue serial = maxBoxWithValue' 2 (emptyValueMap, valueMap) (((0,0), 0), 0)
  where
    valueMap = cellValueMap serial
    maxBoxWithValue' :: Scale -> (CellMap, CellMap) -> (Box, PowerLevel) -> (Box, PowerLevel)
    maxBoxWithValue' scale (mapM2, mapM1) (mBox, mVal) = if scale > mbSIZE
      then (mBox, mVal)
      else maxBoxWithValue' (scale+1) (mapM1, mapM0) (nextBox, nextVal)
        where
          cells = [(x,y)  | x <- [1..(bSIZE-scale+1)],
                            y <- [1..(bSIZE-scale+1)]]
          mapM0 = foldl addCellValue Map.empty cells
          (cCell, cVal) = maxInMap mapM0
          (nextBox, nextVal) = if cVal > mVal
            then ((cCell, scale), cVal)
            else (mBox, mVal)
          addCellValue :: CellMap -> Cell -> CellMap
          addCellValue cellMap (x,y) = Map.insert (x,y) cpl cellMap
            where
              rbBorderLevel =   (mapM1 Map.! (x+1,y+1))
                              - (mapM2 Map.! (x+1,y+1))
                              + (valueMap Map.! (x+scale-1,y))
                              + (valueMap Map.! (x,y+scale-1))
              cpl = (mapM1 Map.! (x,y)) + rbBorderLevel

main :: IO ()
main = do
  s <- readFile "input/day-11.input"
  let serial = read s :: GridSerial
  print $ maxCell serial 3
  print $ maxBox serial


