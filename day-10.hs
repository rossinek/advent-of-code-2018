import System.Environment
import Control.Monad (forM_, mapM_)
import Data.Array (Array, bounds, (!))
import Data.Array.ST (runSTArray)
import Data.Array.MArray (newArray, writeArray)
import Data.List.Split (splitOn)

type Point = (Int, Int)
type Velocity = (Int, Int)
type LightPoint = (Point, Velocity)
type Canvas = Array (Int, Int) Bool

parseInput :: String -> [LightPoint]
parseInput s = do
  l <- lines s
  let (_:posString:veloString:_) = map (head . (splitOn ">")) (splitOn "<" l)
  let (x:y:_) = map read (splitOn "," posString)
  let (u:v:_) = map read (splitOn "," veloString)
  return ((x,y), (u,v))

move :: Point -> Velocity -> Point
move (x,y) (u,v) = (x+u, y+v)

nextSecond :: [LightPoint] -> [LightPoint]
nextSecond sky = do
  (p, v) <- sky
  return (move p v, v)

nextNSeconds :: Int -> [LightPoint] -> [LightPoint]
nextNSeconds 0 sky = sky
nextNSeconds n sky = nextNSeconds (n-1) (nextSecond sky)

toCanvas :: Point -> Point -> [LightPoint] -> Canvas
toCanvas topLeft bottomRight lps = canvasArr
  where
    canvasArr = runSTArray $ do
      arr <- newArray (topLeft, bottomRight) False
      forM_ lps $ \(p, v) -> writeArray arr p True
      return arr

canvasToStrings :: Char -> Char -> Canvas -> [String]
canvasToStrings f t cv = map strLine yCords
  where
    ((minX, minY), (maxX, maxY)) = bounds cv
    xCords = enumFromThenTo minX (minX + 1) maxX
    yCords = enumFromThenTo minY (minY + 1) maxY
    toChar :: Bool -> Char
    toChar False = f
    toChar True = t
    strLine :: Int -> String
    strLine y = map (\x -> toChar $ cv ! (x, y)) xCords

drawCanvas :: Canvas -> IO ()
drawCanvas cv = mapM_ putStrLn $ canvasToStrings '_' 'X' cv

scaleSky :: Int -> [LightPoint] -> [LightPoint]
scaleSky scale sky = map (\((x,y),v) -> ((x `div` scale, y `div` scale), v)) sky

drawSky :: [LightPoint] -> IO ()
drawSky sky = do
  let minX = minimum (map (fst . fst) sky)
  let maxX = maximum (map (fst . fst) sky)
  let minY = minimum (map (snd . fst) sky)
  let maxY = maximum (map (snd . fst) sky)
  let cv = toCanvas (minX, minY) (maxX, maxY) sky
  drawCanvas cv

sizeOf :: [LightPoint] -> Point
sizeOf lps = (w, h)
  where
    minX = minimum (map (fst . fst) lps)
    maxX = maximum (map (fst . fst) lps)
    minY = minimum (map (snd . fst) lps)
    maxY = maximum (map (snd . fst) lps)
    w = maxX - minX
    h = maxY - minY

maximalyCondensedLights :: Int -> [LightPoint] -> (Int, Point)
maximalyCondensedLights maxIter lps = maximalyCondensedLights' (maxIter-1) (nextSecond lps) (0, sizeOf lps)
  where
    maximalyCondensedLights' :: Int -> [LightPoint] -> (Int, Point) -> (Int, Point)
    maximalyCondensedLights' 0 lps (minIndex, minSize) = (minIndex, minSize)
    maximalyCondensedLights' n lps (minIndex, minSize) = if (minSize > currentSize)
      then maximalyCondensedLights' (n-1) (nextSecond lps) (maxIter - n, currentSize)
      else maximalyCondensedLights' (n-1) (nextSecond lps) (minIndex, minSize)
      where
        currentSize = sizeOf lps

main :: IO ()
main = do
  s <- readFile "input/day-10.input"
  let lightingPoints = parseInput s
  let (seconds, _) = maximalyCondensedLights 15000 lightingPoints
  drawSky $ nextNSeconds seconds lightingPoints

