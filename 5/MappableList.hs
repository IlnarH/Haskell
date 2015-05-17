module Program where

data MappableList a b = Nil | Cons a (MappableList b a) deriving (Show, Eq)

empty_list :: MappableList a b
empty_list = Nil

two_elems :: MappableList Int String
two_elems = Cons 1 (Cons "a" empty_list)

function_for_list :: MappableList a b -> c
function_for_list list = case list of 
		Nil -> undefined
		Cons a list -> undefined a (function_for_list list)

list_length :: MappableList a b -> Int
length list = case list of
		Nil -> 0
		Cons a tail -> 1 + list_length tail

map_list :: MappableList a b -> (a -> c) -> (b -> d) -> MappableList c d
map_list list first_fn second_fn = case list of 
		Nil -> Nil
		Cons a tail -> Cons (first_fn a) (map_list tail second_fn first_fn)
	         