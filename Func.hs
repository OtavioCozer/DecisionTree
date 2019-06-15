module Func(
 entropia
)where

entropia :: [Float] -> Float
entropia x = -(sum $ map (\z -> z*(logBase 2 z) ) x)