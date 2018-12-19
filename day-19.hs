import System.Environment
import Data.List.Split (splitOn)
import Helpers.Program

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


main :: IO ()
main = do
  s <- readFile "input/day-19.input"
  let (ipReg, commands) = parseInput s
  let program = initProgram ipReg commands
  print $ runCommands program
