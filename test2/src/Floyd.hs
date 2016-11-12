module Floyd where

import           Data.Array.IO

type Vertex = Int
type Weight = Int
type Graph  = [[(Vertex, Weight)]]
type Dp     = IO (IOArray Vertex (IO (IOArray Vertex Weight)))

readGraph :: Graph
--readGraph = (5, [(0, 1, 10), (1, 3, 4), (0, 2, 5), (2, 4, 2), (4, 3, 5), (2, 3, 20)])
--readGraph = ([0, 1], [(0, 1, 10)])
readGraph = [[(0, 0), (1, 10)]]

--fst3 (a, _, _) = a
--snd3 (_, b, _) = b

getInitDp :: Int -> Dp
getInitDp n = newArray (0, n) (newArray (0, n) 0)

--floyd_warshall start end graph = (dist, [start] ++ route ++ [end])
  --where dist  = shortest (start, end, length graph) graph
        --route = path (start, end, length graph) graph

floyd' :: (Int, Int, Int) -> Graph -> Int
floyd' (i, j, -1) g = snd (g !! i !! j)
floyd' (i, j,  k) g
    | i == j    = 0
    | otherwise = min (floyd' (i, j, k - 1) g) $ (floyd' (i, k, k - 1) g) + (floyd' (k, j, k - 1) g)

--floyd :: Graph -> Dp
floyd = floyd' (0, 1, 1) (readGraph)

