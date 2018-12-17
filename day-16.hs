import System.Environment
import qualified Data.Map.Lazy as Map
import Data.List
import Data.List.Split (splitOn)
import Data.Bits

data Action = Addr | Addi | Mulr | Muli
            | Banr | Bani | Borr | Bori
            | Setr | Seti | Gtir | Gtri
            | Gtrr | Eqir | Eqri | Eqrr deriving Eq
type Example = (Registers, Command, Registers)
type Registers = [Int]
type Command = [Int]
type ActionCall = Action -> Registers -> Command -> Registers
type ActionMap = Map.Map Int Action

parseInput :: String -> ([Example], [Command])
parseInput s = (examples, commands)
  where
    (examples, rest) = consumeExamples (lines s)
    commands = consumeCommands rest
    consumeExamples :: [String] -> ([Example], [String])
    consumeExamples (('B':'e':'f':'o':'r':'e':':':' ':b):c:('A':'f':'t':'e':'r':':':' ':' ':a):next)
      = (((breg, command, areg):nextE), nextR)
        where
          breg = read b
          areg = read a
          command = map read (splitOn " " c)
          (nextE, nextR) = consumeExamples next
    consumeExamples ([]:next) = consumeExamples next
    consumeExamples next = ([], next)

    consumeCommands :: [String] -> [Command]
    consumeCommands [] = []
    consumeCommands ([]:tail) = consumeCommands tail
    consumeCommands (com:tail) = (map read (splitOn " " com) : consumeCommands tail)


readReg :: Registers -> Int -> Int
readReg regs i = regs !! i

writeReg :: Registers -> Int -> Int -> Registers
writeReg (_:rs) 0 val = val : rs
writeReg (r:rs) n val = r : writeReg rs (n-1) val

actions :: [Action]
actions = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

actionCall :: ActionCall
actionCall Addr regs [_, a, b, c] = writeReg regs c $ (readReg regs a) + (readReg regs b)
actionCall Addi regs [_, a, b, c] = writeReg regs c $ (readReg regs a) + b
actionCall Mulr regs [_, a, b, c] = writeReg regs c $ (readReg regs a) * (readReg regs b)
actionCall Muli regs [_, a, b, c] = writeReg regs c $ (readReg regs a) * b
actionCall Banr regs [_, a, b, c] = writeReg regs c $ (readReg regs a) .&. (readReg regs b)
actionCall Bani regs [_, a, b, c] = writeReg regs c $ (readReg regs a) .&. b
actionCall Borr regs [_, a, b, c] = writeReg regs c $ (readReg regs a) .|. (readReg regs b)
actionCall Bori regs [_, a, b, c] = writeReg regs c $ (readReg regs a) .|. b
actionCall Setr regs [_, a, b, c] = writeReg regs c (readReg regs a)
actionCall Seti regs [_, a, b, c] = writeReg regs c a
actionCall Gtir regs [_, a, b, c] = writeReg regs c (if a > (readReg regs b) then 1 else 0)
actionCall Gtri regs [_, a, b, c] = writeReg regs c (if (readReg regs a) > b then 1 else 0)
actionCall Gtrr regs [_, a, b, c] = writeReg regs c (if (readReg regs a) > (readReg regs b) then 1 else 0)
actionCall Eqir regs [_, a, b, c] = writeReg regs c (if a == (readReg regs b) then 1 else 0)
actionCall Eqri regs [_, a, b, c] = writeReg regs c (if (readReg regs a) == b then 1 else 0)
actionCall Eqrr regs [_, a, b, c] = writeReg regs c (if (readReg regs a) == (readReg regs b) then 1 else 0)

isExampleCall :: Example -> Action -> Bool
isExampleCall (eregs, command, eoutregs) action = (actionCall action eregs command) == eoutregs

amountOfTruth :: [Bool] -> Int
amountOfTruth (False:xs) = amountOfTruth xs
amountOfTruth (True:xs) = 1 + (amountOfTruth xs)
amountOfTruth [] = 0

behaveLikeThreeOrMore :: [Example] -> Int
behaveLikeThreeOrMore exs = amountOfTruth $
  map (\e -> (amountOfTruth (map (isExampleCall e) actions)) >= 3) exs

behaveLike :: [Action] -> Example -> [Action]
behaveLike acs ex = filter (isExampleCall ex) acs

exOp :: Example -> Int
exOp (_, (a:_), _) = a

freeOpcodes :: [Example] -> [Int]
freeOpcodes exs = foldl (\ops i -> union ops [exOp i]) [] exs

unambiguousOpcode :: [Example] -> [Action] -> (Int, Action)
unambiguousOpcode exs acs = (opcode, action)
  where
    ops = freeOpcodes exs
    opsExamples = map (\op -> (filter (\ex -> (exOp ex) == op) exs)) ops
    opsLike = map (\oe@(ee:_) -> (exOp ee, foldl (\bl ex -> union bl (behaveLike acs ex)) [] oe)) opsExamples
    (opcode, (action:[])) = head (filter (\(o, as) -> (length as == 1)) opsLike)

figureOutOpcodes :: [Example] -> ActionMap
figureOutOpcodes exs = snd $ figureOutOpcodes' (exs, Map.empty)
  where
    figureOutOpcodes' :: ([Example], ActionMap) -> ([Example], ActionMap)
    figureOutOpcodes' ([], amap) = ([], amap)
    figureOutOpcodes' (exs, amap) = figureOutOpcodes' (exs', amap')
      where
        (opcode, action) = unambiguousOpcode exs (actions \\ (Map.elems amap))
        exs' = filter (\ex -> (exOp ex) /= opcode) exs
        amap' = Map.insert opcode action amap

runCommands :: ActionMap -> Registers -> [Command] -> Registers
runCommands amap regs [] = regs
runCommands amap regs (c@(op:_):cs) = runCommands amap (actionCall (amap Map.! op) regs c) cs

main :: IO ()
main = do
  s <- readFile "input/day-16.input"
  let (examples, commands) = parseInput s
  print $ behaveLikeThreeOrMore examples
  let amap = figureOutOpcodes examples
  print $ runCommands amap [0,0,0,0] commands
