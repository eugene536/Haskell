{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module A where

import           Data.List(tails, inits)
-- вернуть все суффиксы, которые являются префиксами 
-- клевое название - оптимизировал кол-во символов = 79
g :: (Eq a) => [a] -> [[a]]
g s = map fst $ filter (\p -> fst p==snd p) $ zip (tails s) $ reverse $ inits s

testSuffixes = ["", "a", "aa", "ab", "annnna", "aannaa", "nannan"]
expectSuffix = [[""], ["a", ""], ["aa", "a", ""], ["ab", ""], ["annnna", "a", ""], ["aannaa", "aa", "a", ""], ["nannan", "nan", "n", ""]]
resSuffix    = expectSuffix == map g testSuffixes

-- выкинуть каждый k-ый
dropKs :: Integer -> [a] -> [a]
dropKs k = map snd . filter ((/= 0) . (`mod` k) . fst) . zip [1..]  

testDropKs = [(2, [1..10]), (5, [1..10]), (10, [1..9])]
expectDropKs = [[1,3..10],[1, 2, 3, 4, 6, 7, 8, 9],[1..9]]

-- стек с минимумом, pop, push, multipop
data StackS a = StackS
    { stack    :: [a]
    , minStack :: [a] 
    } deriving (Show)

class Stack s a where
    pop      :: s a -> (a, s a)
    push     :: a -> s a -> s a

    multipop :: Int -> s a -> ([a], s a)
    multipop 0 s = ([], s)
    multipop k s = (v : tl, rs)
        where (v, ns) = pop s
              (tl, rs) = multipop (k - 1) ns 
        

instance (Ord a) => Stack StackS a where
    pop (StackS (s:ss) mns@(m:ms)) = (m, StackS ss nMins)
        where nMins = if s == m    
                      then ms
                      else mns

    --push = undefined
    push x (StackS s mns@(m:ms)) = StackS (x:s) nMins
        where nMins = if x <= m    
                      then x:mns
                      else mns




