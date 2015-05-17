module Fibonacci where

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci a = fibonacci(a - 1) + fibonacci(a - 2)