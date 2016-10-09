module ZeroInMin where

import           Data.Maybe(isJust)
import           Data.List(find)
import           Debug.Trace(trace)

manHeaps :: (Int, Int) -> [(Int, Int)]
manHeaps (a, b) = filter isCorrectHeaps
    [ (a - 1, b    ), (a   *   2, b `div` 2)
    , (a    , b - 1), (a `div` 2, b   *   2)
    ]
  where
    isCorrectHeaps (x, y) = x >= 0 && y >= 0

zeroIn3 :: (Int, Int) -> Bool
zeroIn3 h = any (\(a, b) -> a == 0 && b == 0) $
            [h] >>= manHeaps >>= manHeaps >>= manHeaps

zeroInMin :: (Int, Int) -> Int
zeroInMin h = zeroInMinWithCounter [h] 0
    where 
        zeroInMinWithCounter arr cnt 
            | (0, 0) `elem` arr = cnt
            | otherwise         = zeroInMinWithCounter (arr >>= manHeaps) (cnt + 1)

