import System.Environment
import qualified Data.Sequence as Seq

type Scores = Seq.Seq Int
type Elf = Int
type State = (Scores, Elf, Elf)

nextState :: State -> State
nextState (scores, e0, e1) = (newScores, e0', e1')
  where
    c0 = scores `Seq.index` e0
    c1 = scores `Seq.index` e1
    ds = digits (c0 + c1)
    newScores = foldl (Seq.|>) scores ds
    e0' = (e0 + (c0 + 1)) `mod` (Seq.length newScores)
    e1' = (e1 + (c1 + 1)) `mod` (Seq.length newScores)

digits :: Int -> [Int]
digits n = map (read . (replicate 1)) (show n)

mixRecipesUntilN :: Int -> State -> State
mixRecipesUntilN n state@(scores,_,_)
  | n <= Seq.length scores  = state
  | otherwise               = mixRecipesUntilN n (nextState state)

findNRecipesSuffix :: Int -> Int -> State -> Scores
findNRecipesSuffix pos len state = Seq.drop pos recipes
  where (recipes,_,_) = mixRecipesUntilN (pos+len) state

mixRecipesUntilSeq :: Scores -> State -> Int
mixRecipesUntilSeq suff state@(s,_,_)
  | isSuffix suff (Seq.take ((Seq.length s) - 1) s) = (Seq.length s) - (Seq.length suff) - 1
  | isSuffix suff s                                 = (Seq.length s) - (Seq.length suff)
  | otherwise                                       = mixRecipesUntilSeq suff (nextState state)
    where
      isSuffix :: Scores -> Scores -> Bool
      isSuffix b a = Seq.drop ((Seq.length a) - (Seq.length b)) a == b

findNRecipesUntilSeq :: Int -> State -> Int
findNRecipesUntilSeq intSuff state = mixRecipesUntilSeq (Seq.fromList (digits intSuff)) state

main :: IO ()
main = do
  s <- readFile "input/day-14.input"
  let input = read s :: Int
  let initialState = (Seq.fromList [3, 7], 0, 1)
  print $ findNRecipesSuffix 5 10 initialState
  print $ findNRecipesUntilSeq input initialState

