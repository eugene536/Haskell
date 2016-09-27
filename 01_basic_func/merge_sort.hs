module MergeSort where

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge lsl rsl
    where
        merge [] r = r
        merge l [] = l
        merge l@(fl:tl) r@(fr:tr)
            | fl < fr   = fl : merge tl r
            | otherwise = fr : merge l tr

        lsl = mergeSort l_half
        rsl = mergeSort r_half

        l_half  = take half l
        r_half  = drop half l
        half = div (length l) 2

