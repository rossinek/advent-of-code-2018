import System.Environment
import Data.List.Split (splitOn)
import Helpers.Program
import Control.Monad (foldM_)
import qualified Data.Set as Set

type Breakpoints = [Int]

parseInput :: String -> (RegIP, [Command])
parseInput s = (ipReg, commands)
  where
    ls = lines s
    ipReg = read (drop 4 (head ls))
    commands = consumeCommands (tail ls)

    consumeCommands :: [String] -> [Command]
    consumeCommands [] = []
    consumeCommands (com:tail) = consumeCommand com : consumeCommands tail

    consumeCommand :: String -> Command
    consumeCommand com = (opToAction op, read a, read b, read c)
      where (op:a:b:c:[]) = (splitOn " " com)

programState :: Program -> (Int, RegIP, Registers)
programState (Program _ state _) = state
programState (Exitted regs) = (-1, -1, regs)

breakpointInfo :: Program -> ((Int, RegIP, Registers), Command)
breakpointInfo (Program _ state (c:_)) = (state, c)
breakpointInfo (Exitted regs) = ((-1, -1, regs), (Null, 0, 0, 0))

runDebugger :: Breakpoints -> Program -> Program
runDebugger breakpoints program = runDebugger' (runCommand program)
  where
  runDebugger' :: Program -> Program
  runDebugger' (Exitted regs) = Exitted regs
  runDebugger' prog@(Program _ (n,_,_) _) = if n `elem` breakpoints
    then prog
    else runDebugger' (runCommand prog)

-- find first value in register 2
-- at stop condition checking step
minStopValue :: Program -> Int
minStopValue program = readReg regs 2
  where
    (Program _ (_,_,regs) _) = runDebugger [28] program

-- find last unique value in register 2
-- at stop condition checking step
maxStopValue :: Program -> Int
maxStopValue program = findLoop Set.empty (-1) (runDebugger [28] program)
  where
    vars :: Program -> Int
    vars (Exitted regs) = readReg regs 2
    vars (Program _ (_,_,regs) _) = readReg regs 2
    findLoop :: Set.Set Int -> Int -> Program -> Int
    findLoop history prev prog = if ((vars prog) `elem` history)
      then prev
      else findLoop (Set.insert current history) current (runDebugger [28] prog)
        where current = vars prog

main :: IO ()
main = do
  s <- readFile "input/day-21.input"
  let (ipReg, commands) = parseInput s
  let program = initProgramWithRegs ipReg [0,0,0,0,0,0] commands
  print $ minStopValue program
  print $ maxStopValue program
