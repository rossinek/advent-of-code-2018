import System.Environment
-- import qualified Data.Map.Lazy as Map
-- import Data.List.Extras.Argmax
-- import Data.List.Split
import qualified Data.Set as Set
import Data.List

type Task = Char
type Dependency = (Task, Task)

parseDependency :: String -> Dependency
parseDependency line = (line !! 5, line !! 36)

dependOn :: Task -> Dependency -> Bool
dependOn t (x,_) = x == t

isDependent :: Task -> Dependency -> Bool
isDependent t (_,x) = x == t

firstTask :: [Dependency] -> [Task] -> Task
firstTask [] tasks = minimum tasks
firstTask deps tasks = minimum readyTasks
  where
    readyTasks = filter (\t -> not (any (isDependent t) deps)) tasks

tasksOrder :: [Dependency] -> [Task] -> [Task]
tasksOrder [] tasks = sort tasks
tasksOrder deps tasks = (readyTask : tasksOrder (filter (not . (dependOn readyTask)) deps) (filter (/= readyTask) tasks))
  where
    readyTask = firstTask deps tasks

main :: IO ()
main = do
  s <- readFile "day-07.input"
  let dependencies = map parseDependency (lines s)
  let tasks = (Set.toList . Set.fromList) (map fst dependencies ++ map snd dependencies)
  print $ tasks
  print $ tasksOrder dependencies tasks
