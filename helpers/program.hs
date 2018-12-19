module Helpers.Program (
  Program,
  Action,
  Registers,
  RegIP,
  Command,
  ActionCall,
  readReg,
  writeReg,
  actionCall,
  initProgram,
  initProgramWithRegs,
  runCommands,
  runCommand,
  fixIP,
  fixIPLeft,
  fixIPRight,
  opToAction
) where

import Data.List
import Data.Bits

data Program = Exitted Registers | Program [Command] (Int, RegIP, Registers) [Command] deriving Show
data Action = Addr | Addi | Mulr | Muli
            | Banr | Bani | Borr | Bori
            | Setr | Seti | Gtir | Gtri
            | Gtrr | Eqir | Eqri | Eqrr deriving (Eq, Show)
type Registers = [Int]
type RegIP = Int
type Command = (Action, Int, Int, Int)
type ActionCall = Registers -> Command -> Registers

readReg :: Registers -> Int -> Int
readReg regs i = regs !! i

writeReg :: Registers -> Int -> Int -> Registers
writeReg (_:rs) 0 val = val : rs
writeReg (r:rs) n val = r : writeReg rs (n-1) val

actionCall :: ActionCall
actionCall regs (Addr, a, b, c) = writeReg regs c $ (readReg regs a) + (readReg regs b)
actionCall regs (Addi, a, b, c) = writeReg regs c $ (readReg regs a) + b
actionCall regs (Mulr, a, b, c) = writeReg regs c $ (readReg regs a) * (readReg regs b)
actionCall regs (Muli, a, b, c) = writeReg regs c $ (readReg regs a) * b
actionCall regs (Banr, a, b, c) = writeReg regs c $ (readReg regs a) .&. (readReg regs b)
actionCall regs (Bani, a, b, c) = writeReg regs c $ (readReg regs a) .&. b
actionCall regs (Borr, a, b, c) = writeReg regs c $ (readReg regs a) .|. (readReg regs b)
actionCall regs (Bori, a, b, c) = writeReg regs c $ (readReg regs a) .|. b
actionCall regs (Setr, a, b, c) = writeReg regs c (readReg regs a)
actionCall regs (Seti, a, b, c) = writeReg regs c a
actionCall regs (Gtir, a, b, c) = writeReg regs c (if a > (readReg regs b) then 1 else 0)
actionCall regs (Gtri, a, b, c) = writeReg regs c (if (readReg regs a) > b then 1 else 0)
actionCall regs (Gtrr, a, b, c) = writeReg regs c (if (readReg regs a) > (readReg regs b) then 1 else 0)
actionCall regs (Eqir, a, b, c) = writeReg regs c (if a == (readReg regs b) then 1 else 0)
actionCall regs (Eqri, a, b, c) = writeReg regs c (if (readReg regs a) == b then 1 else 0)
actionCall regs (Eqrr, a, b, c) = writeReg regs c (if (readReg regs a) == (readReg regs b) then 1 else 0)

initProgram :: RegIP -> [Command] -> Program
initProgram ipReg cmds = initProgramWithRegs ipReg [0,0,0,0,0,0] cmds

initProgramWithRegs :: RegIP -> Registers -> [Command] -> Program
initProgramWithRegs ipReg regs cmds = Program [] (0, ipReg, regs) cmds

runCommands :: Program -> Program
runCommands (Exitted regs) = Exitted regs
runCommands prog = runCommands (runCommand prog)

runCommand :: Program -> Program
runCommand (Exitted regs) = Exitted regs
runCommand (Program _ (_,_,regs) []) = Exitted regs
runCommand (Program prev (n,ip,regs) (command:next)) = fixIP (Program (command:prev) (n+1,ip,newRegs) next)
  where
    newRegs' = actionCall regs command
    newRegs = writeReg newRegs' ip ((readReg newRegs' ip) + 1)

fixIP :: Program -> Program
fixIP (Exitted regs) = Exitted regs
fixIP prog@(Program prev (n,ip,regs) next) = if (readReg regs ip) < n
  then fixIPLeft prog
  else fixIPRight prog

fixIPLeft :: Program -> Program
fixIPLeft (Exitted regs) = Exitted regs
fixIPLeft prog@(Program [] (n,ip,regs) next) = if (readReg regs ip) == n then prog else Exitted regs
fixIPLeft prog@(Program (c:prev) (n,ip,regs) next) = if (readReg regs ip) == n
  then prog
  else fixIPLeft (Program prev ((n-1),ip,regs) (c:next))

fixIPRight :: Program -> Program
fixIPRight (Exitted regs) = Exitted regs
fixIPRight prog@(Program prev (n,ip,regs) []) = if (readReg regs ip) == n then prog else Exitted regs
fixIPRight prog@(Program prev (n,ip,regs) (c:next)) = if (readReg regs ip) == n
  then prog
  else fixIPRight (Program (c:prev) ((n+1),ip,regs) next)

opToAction :: String -> Action
opToAction op | op == "addr" = Addr
              | op == "addi" = Addi
              | op == "mulr" = Mulr
              | op == "muli" = Muli
              | op == "banr" = Banr
              | op == "bani" = Bani
              | op == "borr" = Borr
              | op == "bori" = Bori
              | op == "setr" = Setr
              | op == "seti" = Seti
              | op == "gtir" = Gtir
              | op == "gtri" = Gtri
              | op == "gtrr" = Gtrr
              | op == "eqir" = Eqir
              | op == "eqri" = Eqri
              | op == "eqrr" = Eqrr
