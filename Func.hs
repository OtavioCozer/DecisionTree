module Func(
 entropia,
 frequency,
 percentagens,
 percentagens'
)where

import Data.List    

entropia :: [Float] -> Float
entropia x = -(sum $ map (\z -> z*(logBase 2 z) ) x)

frequency :: Eq a => a -> [a] -> Int
frequency x xs =   (length . filter (==x)) xs

percentagens :: (Fractional a1, Eq a2) => [a2] -> [a1]
percentagens xs = percentagens' (nub xs) xs (length xs)

percentagens' :: (Fractional a1, Integral t, Eq a2) => [a2] -> [a2] -> t -> [a1]
percentagens' [] _ _ = []
percentagens' (x:xs) list n = ( fromIntegral (frequency x list)) / (fromIntegral n)  : (percentagens' xs list n)