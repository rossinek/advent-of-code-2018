import System.Environment
import Data.Char
import Data.Text (pack, unpack, strip)
import qualified Data.Set as Set

type Unit = Char
type UnitType = Char
type Polymer = [Char]
type PolymerProcessor = Polymer -> Polymer

mapTails :: PolymerProcessor -> Polymer -> Polymer
mapTails processor [] = processor []
mapTails processor (x:xs) = processor (x : mapTails processor xs)

typeOf :: Unit -> UnitType
typeOf unit = toLower unit

areReacting :: Unit -> Unit -> Bool
areReacting x y = (typeOf x == typeOf y) && (x /= y)

headReaction :: Polymer -> Polymer
headReaction (h0:h1:tail) = if areReacting h0 h1
  then tail
  else (h0:h1:tail)
headReaction s = s

polymerReaction :: Polymer -> Polymer
polymerReaction polymer = mapTails headReaction polymer

unitTypes :: Polymer -> Polymer
unitTypes polymer = ((Set.toList . Set.fromList) (map typeOf polymer))

lengthAfterReactions :: Polymer -> Int
lengthAfterReactions input = length (polymerReaction input)

polymerWithoutUnit :: Polymer -> UnitType -> Polymer
polymerWithoutUnit [] c = []
polymerWithoutUnit (h:tail) c = if (typeOf h == c)
  then polymerWithoutUnit tail c
  else (h : polymerWithoutUnit tail c)

main :: IO ()
main = do
  s <- readFile "day-05.input"
  let input =  unpack $ strip $ pack s
  print $ lengthAfterReactions input
  let units = unitTypes input
  print $ minimum (map (lengthAfterReactions . (polymerWithoutUnit input)) units)
