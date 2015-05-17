module YesItCanFly where

data DinnerOrder = Chicken | Pasta | WithoutMeal deriving (Show,Eq)
-- interp. dinner options

function_for_dinner_order :: DinnerOrder -> a
function_for_dinner_order x = case x of
							Chicken -> undefined
							Pasta -> undefined
							WithoutMeal -> undefined

dinner_order_to_msg :: DinnerOrder -> String
dinner_order_to_msg d = case d of
							Chicken -> "The passenger ordered chicken."
							Pasta -> "The passenger ordered pasta."
							NoFood -> "The passenger ordered nothing."