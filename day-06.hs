import System.Environment
import qualified Data.Map.Lazy as Map
import Data.List.Extras.Argmax
import Data.List.Split

type Coordinate = (Int, Int)
type Place = Coordinate
type Board = Map.Map Coordinate (Maybe Place)

parsePlace :: String -> Place
parsePlace s = (read x :: Int, read y :: Int)
  where [x, y] = splitOn ", " s

distance :: Coordinate -> Place -> Int
distance (x, y) (x0, y0) = (abs (x0 - x)) + (abs (y0 - y))

onlyOne :: [a] -> Maybe a
onlyOne l = if length l == 1
  then Just (head l)
  else Nothing

placesMap :: Int -> Int -> [Place] -> Board
placesMap w h places = Map.fromList
  [((x, y), onlyOne (argmins (distance (x, y)) places)) | x <- [0..w+1], y <- [0..h+1]]

hasClosedArea :: Board -> Place -> Bool
hasClosedArea board place = not (any (\b -> (board Map.! b) == Just place) border)
  where
    left = minimum (map fst (Map.keys board))
    top = minimum (map snd (Map.keys board))
    width = maximum (map fst (Map.keys board))
    height = maximum (map snd (Map.keys board))
    border =
      [(x, top) | x <- [0..width]] ++
      [(x, height) | x <- [0..width]] ++
      [(left, y) | y <- [1..(height-1)]] ++
      [(width, y) | y <- [1..(height-1)]]

maxClosedArea :: Board -> [Place] -> Maybe Int
maxClosedArea board [] = Nothing
maxClosedArea board places = if length placesWithClosedArea > 0
  then Just (maximum (map (\place -> length (filter (== place) (Map.elems board))) placesWithClosedArea))
  else Nothing
  where
    placesWithClosedArea = map Just (filter (hasClosedArea board) places)

main :: IO ()
main = do
  s <- readFile "day-06.input"
  let places = map parsePlace (lines s)
  let width = maximum (map fst places)
  let height = maximum (map snd places)
  let board = placesMap width height places
  print $ maxClosedArea board places
