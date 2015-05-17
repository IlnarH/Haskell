module Temperature where

type OatmealTemp = Int

low, normal, high :: OatmealTemp

low = 0
normal = 10
high = 20

function_for_temp :: OatmealTemp -> a
function_for_temp a 
		| a >= 0 && a <= 20 = undefined t


data Adjustment = Left | Current | Right deriving (Show,Eq)

function_for_level :: Adjustment -> a
function_for_level t = case t of
		Left -> undefined
		Current -> undefined
		Right -> undefined


oatmeal_temp_to_adjustment :: OatmealTemp -> Adjustment
oatmeal_temp_to_adjustment temp = if temp < 0 || temp > 20
		then error "Temperature is out of bounds"
		else if temp < normal
				then Right
				else if temp > normal
						then Left
						else Current 

