module Lca where

import           Data.List   (find, intersect)
import           Debug.Trace (trace)

data InNode a = Node
            { label  :: a
            , parent :: Maybe (InNode a)
            } deriving (Show, Eq)

lca :: (Eq a, Show a) => InNode a -> InNode a -> Maybe (InNode a)
lca a b = commonPath >>= Just . fst
    where
        pathA      = getPath a []
        pathB      = getPath b []
        pathAB     = reverse $ zip pathA pathB
        commonPath = find (uncurry (==)) pathAB

getPath :: InNode a -> [InNode a] -> [InNode a]
getPath node@(Node lbl Nothing)  lst = node:lst
getPath node@(Node lbl (Just b)) lst = getPath b (node:lst)

root1   = Node 5 Nothing
ch1_1   = Node 6 $ Just root1
ch1_2   = Node 3 $ Just root1
ch1_1_1 = Node 7 $ Just ch1_1
ch1_1_2 = Node 1 $ Just ch1_1

root2   = Node 2 Nothing
ch2_1   = Node 9  $ Just root2
ch2_2   = Node 10 $ Just root2
ch2_1_1 = Node 8  $ Just ch2_1
ch2_1_2 = Node 4  $ Just ch2_1

