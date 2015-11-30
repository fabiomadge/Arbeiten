module GSA where

import qualified DMapArr as DM
import qualified Data.Map.Strict as M
import Data.Array
import Data.List
import Data.Maybe
import Data.Ord

type Man = Int
type Woman = Int
type Proposals = DM.DMap Man Woman
type Problem = ([[Woman]],[[Man]])
type ManProgress = (Man, [Woman])
type Cemetery = M.Map Man [Woman]
type Queue = ([ManProgress], Cemetery)
type Ranking = Array (Int,Int) (Maybe Int)
type Matching = [(Man, Woman)]
type State = (Proposals, Queue)

sampleProblem :: Problem
sampleProblem = ([
    [5,7,1,2,6,8,4,3],
    [2,3,7,5,4,1,8,6],
    [8,5,1,4,6,2,3,7],
    [3,2,7,4,1,6,8,5],
    [7,2,5,1,3,6,8,4],
    [1,6,7,5,8,4,2,3],
    [2,5,7,6,3,4,8,1],
    [3,8,4,5,7,2,6,1]
  ],[
    [5,3,7,6,1,2,8,4],
    [8,6,3,5,7,2,1,4],
    [1,5,6,2,4,8,7,3],
    [8,7,3,2,4,1,5,6],
    [6,4,7,3,8,1,2,5],
    [2,8,5,3,4,6,7,1],
    [7,5,2,1,8,6,4,3],
    [7,4,1,5,2,3,6,8]
  ])

buildRanking :: Problem -> Ranking
buildRanking (mprfs,wprfs) = let
  wl = length wprfs
  ml = length mprfs
  in array ((1,1),(wl, ml)) [((wi,mi), Just mp) |
    (wi,wl) <- zip [1..wl] wprfs, (mi,mp) <- sortBy (comparing fst) (zip wl [1..length wl])]

depleteQueue :: Ranking -> State -> State
depleteQueue r s@(_,([],_)) = s
depleteQueue r (p,((m,w:ws):ms,c))
  | not (DM.member w p) = depleteQueue r (DM.insert (m, w) p, (ms, M.insert m ws c))
  | highterPrio = let p' = DM.rearrange (m, w) p
    in depleteQueue r (p', (ms ++ [(m', ws')], M.insert m ws (M.delete m' c)))
  | otherwise = depleteQueue r (p,((m,ws):ms,c))
  where
    m' = fst $ fromJust $ DM.lookupWithB w p
    ws' = (M.!) c m'
    highterPrio = maybe False (\x -> x < fromJust (r ! (w, m'))) (r ! (w,m))

findStableMatching :: Problem -> Matching
findStableMatching p@(m,_) = DM.toList props
  where
    manProgress = (zip [1..(length m)] m, M.empty)
    (props,_) = depleteQueue (buildRanking p) (DM.empty (length m), manProgress)
