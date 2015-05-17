module Ackermann where

ackermann :: Int -> Int -> Int
ackermann a b = if (a < 0) || (b < 0)
		then error "Negative number"
		else if a == 0 
				then b + 1
				else if b == 0
					then ackermann (a - 1) 1
					else ackermann (a - 1) (ackermann a (b - 1))
