module Moving where

import           Control.Monad.State (State, evalState, get, put, replicateM)
import           Data.List           (genericLength)

movState :: State (Int, [Double]) Double
movState =
    get >>= \(n, arr) ->
    put (n, drop 1 arr) >>
    let first_n = take n arr
        avgN    = sum first_n / genericLength first_n in
    return avgN

moving :: Int -> [Double] -> [Double]
moving w arr = reverse resRev
    where n      = length arr
          revArr = reverse arr
          resRev = evalState (replicateM n movState) (w, revArr)

test1 = moving 4 [1, 5, 3, 8, 7, 9, 6]
test2 = moving 2 [1, 5, 3, 8, 7, 9, 6]
