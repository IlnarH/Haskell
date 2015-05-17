module Program where

data BinaryTree = Nil | Cons Int BinaryTree BinaryTree deriving(Show)

empty :: BinaryTree
empty = Nil

one_elem :: BinaryTree
one_elem = Cons 1 empty_tree empty_tree

two_elems :: BinaryTree
two_elems = Cons 1 empty_tree (Cons 2 empty_tree empty_tree)

function_for_tree :: BinaryTree -> a
function_for_tree tree = case tree of
		Nil -> undefined
		Cons value left right -> undefined value (function_for_tree left) (function_for_tree right)

height :: BinaryTree -> Int
height tree = case tree of
		Nil -> 0
		Cons value left right -> 1 + (max (tree_height left) (tree_height right))

tree_elems_sum :: BinaryTree -> Int
tree_elems_sum tree = case tree of 
		Nil -> 0
		Cons value left right -> value + (tree_elems_sum left) + (tree_elems_sum right)

tree_search :: BinaryTree -> Int -> Bool
tree_search tree searchingValue = case tree of 
		Nil -> False
		Cons value left right -> if value == searchingValue
										then True
								else if searchingValue < value
										then tree_search left searchingValue
									else tree_search right searchingValue