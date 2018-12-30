import System.Environment
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.List.Extra (maximumOn)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map

type UnitGroup = (GroupType, Quantity, HitPoints, AttackDamage, AttackType, Initiative, Weaknesses, Immunities)
type Quantity = Int
type HitPoints = Int
type AttackDamage = Int
type Initiative = Int
type AttackType = String
type Weaknesses = Set.Set AttackType
type Immunities = Set.Set AttackType
type GroupID = Int
data GroupType = Immune | Infection deriving (Eq, Show)

parseInput :: String -> Map.Map GroupID UnitGroup
parseInput s = Map.fromList (map (\g -> (gID g, g)) unitGroups)
  where
    immuneStrings = drop 1 (takeWhile (/="") (lines s))
    infectionStrings = drop 2 (dropWhile (/="") (lines s))
    unitGroups = (map (parseUnitGroup Immune) immuneStrings) ++ (map (parseUnitGroup Infection) infectionStrings)
    parseUnitGroup :: GroupType -> String  -> UnitGroup
    parseUnitGroup gt s = (gt, unitsCount, hp, damage, attackType, initiative, weaknesses, immunities)
      where
        unitsCount = read (head (words s)) :: Int
        hp = read (head (drop 4 (words s))) :: Int
        (_:attackString:_) = splitOn "with an attack that does " s
        damage = read (head (words attackString)) :: Int
        attackType = (head (drop 1 (words attackString)))
        initiative = read (head (drop 5 (words attackString))) :: Int
        insideBrackets = takeBetween '(' ')' s
        weaknessesAndImmunities = splitOn "; " insideBrackets
        weaknessesStrings = filter (\str -> (take 4 str) == "weak") (splitOn "; " insideBrackets)
        immunitiesStrings = filter (\str -> (take 6 str) == "immune") (splitOn "; " insideBrackets)
        weaknesses = if null weaknessesStrings
          then Set.empty
          else Set.fromList (splitOn ", " (drop 8 (head weaknessesStrings)))
        immunities = if null immunitiesStrings
          then Set.empty
          else Set.fromList (splitOn ", " (drop 10 (head immunitiesStrings)))
    takeBetween :: Char -> Char -> String -> String
    takeBetween c0 c1 s = drop 1 (takeWhile (/= c1) (dropWhile (/= c0) s))

gType :: UnitGroup -> GroupType
gType (gt, _, _, _, _, _, _, _) = gt
gQuantity :: UnitGroup -> Quantity
gQuantity (_, q, _, _, _, _, _, _) = q
gHP :: UnitGroup -> HitPoints
gHP (_, _, hp, _, _, _, _, _) = hp
gAttackDamage :: UnitGroup -> AttackDamage
gAttackDamage (_, _, _, ad, _, _, _, _) = ad
gAttackType :: UnitGroup -> AttackType
gAttackType (_, _, _, _, at, _, _, _) = at
gInitiative :: UnitGroup -> Initiative
gInitiative (_, _, _, _, _, i, _, _) = i
gWeaknesses :: UnitGroup -> Weaknesses
gWeaknesses (_, _, _, _, _, _, ws, _) = ws
gImmunities :: UnitGroup -> Immunities
gImmunities (_, _, _, _, _, _, _, is) = is
gEffectivePower :: UnitGroup -> AttackDamage
gEffectivePower g = (gQuantity g) * (gAttackDamage g)
gID :: UnitGroup -> GroupID
gID = gInitiative
gUpdateQuantity :: UnitGroup -> Quantity -> UnitGroup
gUpdateQuantity (gt, q, hp, ad, at, i, ws, is) q' = (gt, q', hp, ad, at, i, ws, is)

attackDamage :: UnitGroup -> UnitGroup -> AttackDamage
attackDamage attacker defender = if (gAttackType attacker) `Set.member` (gImmunities defender)
  then 0
  else if (gAttackType attacker) `Set.member` (gWeaknesses defender)
    then (gEffectivePower attacker) * 2
    else gEffectivePower attacker

attack :: UnitGroup -> UnitGroup -> UnitGroup
attack attacker defender = gUpdateQuantity defender ((gQuantity defender) - ((attackDamage attacker defender) `div` (gHP defender)))

fight :: Map.Map GroupID UnitGroup -> Map.Map GroupID UnitGroup
fight groups = attacking groups (targetSelection groups)

targetSelection :: Map.Map GroupID UnitGroup -> Map.Map GroupID GroupID
targetSelection groups
    = foldl selectTarget Map.empty (reverse (sortOn (\g -> (gEffectivePower g, gInitiative g)) groupsList))
  where
    groupsList = Map.elems groups
    selectTarget :: Map.Map GroupID GroupID -> UnitGroup -> Map.Map GroupID GroupID
    selectTarget selections group = if null opponents
      then selections
      else Map.insert (gID group) (gID (maximumOn (\g -> (attackDamage group g, gEffectivePower g, gInitiative g)) opponents)) selections
      where
        opponents = filter (\g -> (gType g /= gType group) && not ((gID g) `elem` (Map.elems selections))) groupsList

attacking :: Map.Map GroupID UnitGroup -> Map.Map GroupID GroupID -> Map.Map GroupID UnitGroup
attacking groupsMap selection = foldl attackTarget groupsMap (reverse (sortOn gInitiative groupsList))
  where
    groupsList = Map.elems groupsMap
    attackTarget :: Map.Map GroupID UnitGroup -> UnitGroup -> Map.Map GroupID UnitGroup
    attackTarget groups g = if isNothing group || isNothing selectedTargetID
      then groups
      else if gQuantity targetAfterAttack <= 0
        then Map.delete (gID targetAfterAttack) groups
        else Map.insert (gID targetAfterAttack) targetAfterAttack groups
      where
        group = groups Map.!? (gID g)
        selectedTargetID = selection Map.!? (gID (fromJust group))
        targetAfterAttack = attack (fromJust group) (groups Map.! (fromJust selectedTargetID))

anyOpponents :: Map.Map GroupID UnitGroup -> Bool
anyOpponents groups = hasOpponent (gType (head glist)) (tail glist)
  where
    glist = (Map.elems groups)
    hasOpponent :: GroupType -> [UnitGroup] -> Bool
    hasOpponent gt [] = False
    hasOpponent gt (u:us) = if gt /= gType u then True else hasOpponent gt us

combat :: Map.Map GroupID UnitGroup -> Map.Map GroupID UnitGroup
combat groups = if anyOpponents groups
  then combat (fight groups)
  else groups

unitsCount :: Map.Map GroupID UnitGroup -> Quantity
unitsCount groups = sum (map gQuantity (Map.elems groups))


main :: IO ()
main = do
  s <- readFile "input/day-24.input"
  let input = parseInput s
  print $ unitsCount (combat input)
