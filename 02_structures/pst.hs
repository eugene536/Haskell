{-# LANGUAGE RecordWildCards #-}

module Pst where

import           Data.List  (sort, minimumBy)
import           Debug.Trace(trace)
import           TreePrinters(verticalPrint, Tree(..))

data Point xT yT = Point
    { x :: xT
    , y :: yT
    } deriving (Show, Eq)

instance (Ord xT, Ord yT) => Ord (Point xT yT) where
    (Point x y) <= (Point x2 y2) = (x, y) <= (x2, y2)

data NodeValue pT xT yT = NodeValue
    { x_mid :: xT
    , prior :: pT xT yT
    } deriving (Show)

type Pst pT xT yT = Tree (NodeValue pT xT yT)
type PII  = Point Int Int
type PstI = Pst Point Int Int

arr2points :: [(a, b)] -> [Point a b]
arr2points = map (\x -> Point (fst x) (snd x))

points :: [PII]
points = arr2points $ [(3, 6), (2, 4), (5, 4), (0, 3), (8, 5), (1, 1), (7, 2)]

createPst :: (Ord a, Ord b) => [Point a b] -> Pst Point a b
createPst []  = Leaf
createPst [p] = Node (NodeValue (x p) p) Leaf Leaf
createPst arr = Node (NodeValue (x mid_el_x) min_el_y) (createPst l_arr) (createPst r_arr)
    where 
        min_el_y  = minimumBy (\a b -> compare (y a, x a) (y b, x b)) arr
        sortedArr = sort $ filter (/= min_el_y) arr
        half      = length sortedArr `div` 2
        mid_el_x  = sortedArr !! half
        l_arr     = filter (<= mid_el_x) sortedArr
        r_arr     = filter (> mid_el_x) sortedArr


-- get all points from semi rectangle
-- y_r ------------------- y_r
--  |                       |
--  |                       |
--  |                       |
--  |                       |
-- x_l                     x_r
rangeSearchPst :: (Ord a, Show a, Ord b, Show b) => Point a b -> Point a b -> Pst Point a b -> [Point a b]
rangeSearchPst l r pst  = rangeSearchPstHelper []  pst
    where 
        l_x = x l
        r_x = x r
        r_y | y l == y r = y l
            | otherwise  = error "y's must be equal"

        rangeSearchPstHelper :: (Ord a, Show a, Ord b, Show b) => [Point a b] -> Pst Point a b -> [Point a b]
        rangeSearchPstHelper res Leaf = res
        rangeSearchPstHelper res pst@(Node (NodeValue x_mid p@(Point xc yc)) lt rt)
            | yc > r_y     = res
            | x_mid <= l_x = rangeSearchPstHelper res_n rt
            | x_mid > r_x  = rangeSearchPstHelper res_n lt
            | otherwise    = res_n ++ 
                             rangeSearchPstHelper [] lt ++
                             rangeSearchPstHelper [] rt
            where
                res_n
                    | l_x <= xc && xc <= r_x = p:res
                    | otherwise              = res



