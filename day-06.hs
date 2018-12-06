import System.Environment
import qualified Data.Map.Lazy as Map
import Data.List.Extras.Argmax
import Data.List.Split

type Place = (Int, Int)
type Board a = Map.Map Place a

parsePlace :: String -> Place
parsePlace s = (read x :: Int, read y :: Int)
  where [x, y] = splitOn ", " s

distance :: Place -> Place -> Int
distance (x, y) (x0, y0) = (abs (x0 - x)) + (abs (y0 - y))

onlyOne :: [a] -> Maybe a
onlyOne l = if length l == 1
  then Just (head l)
  else Nothing

placesMap :: Int -> Int -> (Place -> a) -> [Place] -> Board a
placesMap w h f places = Map.fromList
  [((x, y), f (x, y)) | x <- [0..w+1], y <- [0..h+1]]

minUniqueDistanceMap :: Int -> Int -> [Place] -> Board (Maybe Place)
minUniqueDistanceMap w h places = placesMap w h (\place -> onlyOne (argmins (distance place) places)) places

totalDistanceMap :: Int -> Int -> [Place] -> Board Int
totalDistanceMap w h places = placesMap w h (\place -> sum (map (distance place) places)) places

hasClosedArea :: Board (Maybe Place) -> Place -> Bool
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

maxClosedArea :: Board (Maybe Place) -> [Place] -> Maybe Int
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
  let minDistBoard = minUniqueDistanceMap width height places
  print $ maxClosedArea minDistBoard places
  let totalDistBoard = totalDistanceMap width height places
  print $ length (filter (<10000) (Map.elems totalDistBoard))
