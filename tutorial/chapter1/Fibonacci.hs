module Fibonacci where

fibs :: [Int]
fibs = fibgen 0 1
  where
    fibgen :: Int -> Int -> [Int]
    fibgen n m = n : fibgen m (n + m)
