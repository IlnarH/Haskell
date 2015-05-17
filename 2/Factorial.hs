module Factorial where

factorial :: Int -> Int
factorial 0 = 1
factorial a = if (a > 0)
		then a * factorial(a - 1)
		else error "Negative number"
	
	