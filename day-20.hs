import System.Environment
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

type Edge = (Vertex, Vertex)
type Vertex = (Int, Int)
type EdgeSet = Set.Set Edge
type VertexSet = Set.Set Vertex
type Regex = String

parseInput :: String -> Regex
parseInput s = init (tail (head (lines s)))

createEdgeSet :: Regex -> EdgeSet
createEdgeSet regex = snd (edgeSet regex (Set.singleton (0,0)) Set.empty)

edgeSet :: Regex -> VertexSet -> EdgeSet -> (VertexSet, EdgeSet)
edgeSet [] vset eset = (vset, eset)
edgeSet rx@('(':_) vset eset = edgeSet rest vset' eset'
  where
    (vset', eset', rest) = consumeBrackets rx vset eset
edgeSet (direction:rx) vset eset = edgeSet rx vset' eset'
  where
    vset' = Set.mapMonotonic (moveVertex direction) vset
    newEset = Set.fromList $ zip (Set.toAscList vset) (Set.toAscList vset')
    eset' = Set.union eset newEset

consumeBrackets :: Regex -> VertexSet -> EdgeSet -> (VertexSet, EdgeSet, Regex)
consumeBrackets ('(':ts) vset eset = (newVset, newEset, rest)
  where
    (options, rest) = consumeBrackets' [""] 1 ts
    (newVset, newEset) = foldl addOption (vset, eset) options
    addOption :: (VertexSet, EdgeSet) -> Regex -> (VertexSet, EdgeSet)
    addOption (accVs, accEs) regex = (accVs', accEs')
      where
        (vs, accEs') = edgeSet regex vset accEs
        accVs' = Set.union accVs vs
    consumeBrackets' :: [String] -> Int -> Regex -> ([String], String)
    consumeBrackets' (l:ls) 1 (')':ts) = ((reverse l):ls, ts)
    consumeBrackets' (l:ls) 1 ('|':ts) = consumeBrackets' ("":(reverse l):ls) 1 ts
    consumeBrackets' (l:ls) n (')':ts) = consumeBrackets' ((')':l):ls) (n-1) ts
    consumeBrackets' (l:ls) n ('(':ts) = consumeBrackets' (('(':l):ls) (n+1) ts
    consumeBrackets' (l:ls) n (c:ts) = consumeBrackets' ((c:l):ls) n ts

moveVertex :: Char -> Vertex -> Vertex
moveVertex 'N' (x, y) = (x,   y+1)
moveVertex 'E' (x, y) = (x+1, y  )
moveVertex 'S' (x, y) = (x,   y-1)
moveVertex 'W' (x, y) = (x-1, y  )

isAdjacent :: Vertex -> Edge -> Bool
isAdjacent v (s, t) = s == v || t == v

graphNeighbours :: Vertex -> EdgeSet -> VertexSet
graphNeighbours v emap = Set.map neighbour (Set.filter (isAdjacent v) emap)
  where
    neighbour :: (Vertex, Vertex) -> Vertex
    neighbour (s, t) = if s == v then t else s

vertexSet :: EdgeSet -> VertexSet
vertexSet eset = foldl (\vset (u,v) -> Set.insert v (Set.insert u vset)) Set.empty eset

-- BFS
farthestVertex :: Vertex -> EdgeSet -> Int
farthestVertex v es = farthestVertex' (Set.empty) es (Seq.singleton (v, 0))
-- BFS
farthestVertex' :: VertexSet -> EdgeSet -> Seq.Seq (Vertex, Int) -> Int
farthestVertex' visited eset queue = if Seq.null queue' then n else farthestVertex' visited' eset queue'
  where
    (v, n) = queue `Seq.index` 0
    visited' = if v `Set.member` visited then visited else Set.insert v visited
    notVisitedNeighbours = Set.difference (graphNeighbours v eset) visited
    queue' = foldl (\q u -> q Seq.|> (u, n+1)) (Seq.drop 1 queue) (Set.toList notVisitedNeighbours)

-- BFS
hasLongestShortestPath :: Vertex -> EdgeSet -> Int -> VertexSet
hasLongestShortestPath v es k = hasLongestShortestPath' (Set.empty) es (Seq.singleton (v, 0)) k
-- BFS
hasLongestShortestPath' :: VertexSet -> EdgeSet -> Seq.Seq (Vertex, Int) -> Int -> VertexSet
hasLongestShortestPath' visited eset queue k = if n == k
  then Set.difference (vertexSet eset) visited
  else if Seq.null queue'
    then Set.empty
    else hasLongestShortestPath' visited' eset queue' k
      where
        (v, n) = queue `Seq.index` 0
        visited' = if v `Set.member` visited then visited else Set.insert v visited
        notVisitedNeighbours = Set.difference (graphNeighbours v eset) visited
        queue' = foldl (\q u -> q Seq.|> (u, n+1)) (Seq.drop 1 queue) (Set.toList notVisitedNeighbours)

main :: IO ()
main = do
  s <- readFile "input/day-20.input"
  let input = parseInput s
  let edgeSet = createEdgeSet input
  print $ farthestVertex (0,0) edgeSet
  print $ Set.size (hasLongestShortestPath (0,0) edgeSet 1000)

