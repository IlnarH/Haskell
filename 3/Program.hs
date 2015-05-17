module Program where

fibonacci :: Int -> Int
fibonacci a
		| a == 0 = 0
		| otherwise = fibonacci_func a 1 0
				where 
						fibonacci_func 0 y z = y
						fibonacci_func x y z = fibonacci_func (x - 1) (y + z) y



binomial :: Int -> Int -> Int
binomial a b = binomial_func 1 1 a b
  		where
	    		binomial_func x y z 0 = x `div` y
    			binomial_func x y 0 k = 0
    			binomial_func x y z k = binom' (x * z) (y * k) (z - 1) (k - 1)
