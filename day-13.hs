import System.Environment
import Control.Monad (mapM_)
import Data.Array (Array, array, bounds, (!))
import Data.List (sortOn)

data Direction = UP | DOWN | LEFT | RIGHT deriving Show
data NextTurn = CL | CS | CR deriving Show
type Position = (Int, Int)
type Track = Char
type TracksMap = Array Position Track
type Cart = (Position, Direction, NextTurn)
type Crash = Position

parseInput :: String -> (TracksMap, [Cart])
parseInput s = (array ((0, 0), (width-1, height-1)) values, carts)
  where
    ls = lines s
    width = length (head ls)
    height = length ls
    values' = [((x,y), (ls !! y) !! x) | x <- [0..(width-1)], y <- [0..(height-1)]]
    (values, carts) = separateCarts values' []

    separateCarts :: [(Position, Char)] -> [Cart] -> ([(Position, Char)], [Cart])
    separateCarts ((pos,'^'):ts) carts = (((pos,'|'):ts'), carts')
      where (ts', carts') = separateCarts ts ((pos,UP,CL):carts)
    separateCarts ((pos,'v'):ts) carts = (((pos,'|'):ts'), carts')
      where (ts', carts') = separateCarts ts ((pos,DOWN,CL):carts)
    separateCarts ((pos,'>'):ts) carts = (((pos,'-'):ts'), carts')
      where (ts', carts') = separateCarts ts ((pos,RIGHT,CL):carts)
    separateCarts ((pos,'<'):ts) carts = (((pos,'-'):ts'), carts')
      where (ts', carts') = separateCarts ts ((pos,LEFT,CL):carts)
    separateCarts (x:ts) carts = ((x:ts'), carts')
      where (ts', carts') = separateCarts ts carts
    separateCarts [] carts = ([], carts)

mapToStrings :: TracksMap -> [String]
mapToStrings tm = map strLine yCords
  where
    (_, (w, h)) = bounds tm
    xCords = enumFromThenTo 0 1 w
    yCords = enumFromThenTo 0 1 h
    strLine :: Int -> String
    strLine y = map (\x -> tm ! (x, y)) xCords

drawCanvas :: TracksMap -> IO ()
drawCanvas tm = mapM_ putStrLn $ mapToStrings tm

nextTick :: TracksMap -> [Cart] -> ([Cart], [Crash])
nextTick tmap carts = nextTick' (sortCarts carts) [] []
  where
    nextTick' :: [Cart] -> [Cart] -> [Crash] -> ([Cart], [Crash])
    nextTick' [] moved crashes = (moved, crashes)
    nextTick' (cart:cs) moved crashes = nextTick' nextCarts nextMoved nextCrashes
      where
        nextC = moveCart tmap cart
        (nextCarts, cr) = afterCrashes nextC cs []
        (moved', newCrashes) = afterCrashes nextC moved cr
        nextMoved = if length newCrashes > 0 then moved' else (nextC:moved')
        nextCrashes = newCrashes ++ crashes

    afterCrashes :: Cart -> [Cart] -> [Crash] -> ([Cart], [Crash])
    afterCrashes c [] crashes = ([], crashes)
    afterCrashes c@(pos, _, _) (c'@(pos', _, _):cs) crashes = (nextCarts, nextCrashes)
      where
        isCrash = pos == pos'
        crashes' = if isCrash then (pos:crashes) else crashes
        (cs', nextCrashes) = afterCrashes c cs crashes'
        nextCarts = if isCrash then cs' else (c':cs')

sortCarts :: [Cart] -> [Cart]
sortCarts carts = sortOn (\((x,y),_,_) -> (y,x)) carts

moveCart :: TracksMap -> Cart -> Cart
moveCart tmap ((x,y), UP, tdir) = adaptDirection (tmap ! (x,y-1)) ((x,y-1), UP, tdir)
moveCart tmap ((x,y), DOWN, tdir) = adaptDirection (tmap ! (x,y+1)) ((x,y+1), DOWN, tdir)
moveCart tmap ((x,y), LEFT, tdir) = adaptDirection (tmap ! (x-1,y)) ((x-1,y), LEFT, tdir)
moveCart tmap ((x,y), RIGHT, tdir) = adaptDirection (tmap ! (x+1,y)) ((x+1,y), RIGHT, tdir)

adaptDirection :: Track -> Cart -> Cart
adaptDirection '-' c = c
adaptDirection '|' c = c
adaptDirection '/'  (p, LEFT,  nt) = (p, DOWN,  nt)
adaptDirection '/'  (p, UP,    nt) = (p, RIGHT, nt)
adaptDirection '/'  (p, RIGHT, nt) = (p, UP,    nt)
adaptDirection '/'  (p, DOWN,  nt) = (p, LEFT,  nt)
adaptDirection '\\' (p, LEFT,  nt) = (p, UP,    nt)
adaptDirection '\\' (p, UP,    nt) = (p, LEFT,  nt)
adaptDirection '\\' (p, RIGHT, nt) = (p, DOWN,  nt)
adaptDirection '\\' (p, DOWN,  nt) = (p, RIGHT, nt)
adaptDirection '+'  (p, dir, nt) = (p, turn nt dir, nextTurn nt)

nextTurn :: NextTurn -> NextTurn
nextTurn CL = CS
nextTurn CS = CR
nextTurn CR = CL

turn :: NextTurn -> Direction -> Direction
turn CS dir   = dir
turn CL LEFT  = DOWN
turn CL UP    = LEFT
turn CL RIGHT = UP
turn CL DOWN  = RIGHT
turn CR LEFT  = UP
turn CR UP    = RIGHT
turn CR RIGHT = DOWN
turn CR DOWN  = LEFT

firstCrash :: TracksMap -> [Cart] -> Crash
firstCrash tmap carts = if length crashes > 0 then last crashes else firstCrash tmap nextCarts
  where (nextCarts, crashes) = nextTick tmap carts

lastCartPosition :: TracksMap -> [Cart] -> Position
lastCartPosition tmap ((pos,_,_):[]) = pos
lastCartPosition tmap carts = lastCartPosition tmap (fst (nextTick tmap carts))
  where (nextCarts, crashes) = nextTick tmap carts

main :: IO ()
main = do
  s <- readFile "input/day-13.input"
  let (tracksMap, carts) = parseInput s
  print $ firstCrash tracksMap carts
  print $ lastCartPosition tracksMap carts
