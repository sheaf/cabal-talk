module Lib where

-- Imagine some function that would compile to much faster code
-- were we able to make use of the AVX2 instruction set.
libFun :: Double -> Double
libFun = (+1)
