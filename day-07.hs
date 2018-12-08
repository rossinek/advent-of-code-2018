import System.Environment
import qualified Data.Set as Set
import Data.List
import Data.List.Extra (minimumOn)
import Data.Char (ord)
import Data.Graph.Inductive.Query.Monad (mapFst)

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

type TaskWithDependencies = (Task, RemainingTime, [Task])
type RemainingTime = Int
type Workers = Int
type Done = Task
type InProgress = (Task, RemainingTime)
type ToDo = TaskWithDependencies
type Step = ([Done], [InProgress], [ToDo])

readyQueue :: [TaskWithDependencies] -> [TaskWithDependencies]
readyQueue tasks = sortOn (\(t, rt, deps) -> (length deps, t, rt)) readyTasks
  where
    readyTasks = filter (\(_,_,d) -> d == []) tasks

groupDependencies :: [Dependency] -> [Task] -> [TaskWithDependencies]
groupDependencies deps tasks = map (\t -> (t, reminingTime t, taskDependencies t)) tasks
  where
    taskDependencies :: Task -> [Task]
    taskDependencies t = map fst (filter (isDependent t) deps)
    reminingTime :: Task -> RemainingTime
    reminingTime t = ((ord t) - 4)

tick :: Workers -> Step -> [Step]
tick w (done, [], []) = []
tick w (d, ip, td) = (newStep : tick w newStep)
  where
    nextInProgress = map (\(t, rt) -> (t, (rt-1))) ip
    (newDone, stillInProgress) = mapFst (map fst) (partition (\(_, rt) -> rt<=0) nextInProgress)
    freeWorkers = w - (length stillInProgress)

    nextToDo = map (\(t, rt, deps) -> (t, rt, deps \\ newDone )) td
    willStart = take freeWorkers (readyQueue nextToDo)
    stillToDo = nextToDo \\ willStart
    newInProgress = map (\(t,rt,ts) -> (t,rt)) willStart

    newStep = (d ++ newDone, stillInProgress ++ newInProgress, stillToDo)

constructTimeline :: [TaskWithDependencies] -> Workers -> [Step]
constructTimeline tasksWithDeps w = tick w ([], [], tasksWithDeps)

main :: IO ()
main = do
  s <- readFile "input/day-07.input"
  let dependencies = map parseDependency (lines s)
  let tasks = (Set.toList . Set.fromList) (map fst dependencies ++ map snd dependencies)
  print $ tasksOrder dependencies tasks
  print $ (length (constructTimeline (groupDependencies dependencies tasks) 5)) - 1
