module Program where

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

one_elem :: Tree Int
one_elem = Leaf 1

two_elems :: Tree String
two_elems = Branch (Leaf "a") (Leaf "b")

function_for_tree :: Tree a -> b
function_for_tree tree = case tree of
		Leaf a -> undefined a
		Branch left right -> (function_for_tree left) (function_for_tree right)

height :: Tree a -> Int
height tree = case tree of
		Leaf a -> 1
		Branch left right -> 1 + (max (tree_height left) (tree_height right))

map_tree :: Tree a -> (a -> b) -> Tree b
map_tree tree fn = case tree of
		Leaf a -> Leaf (fn a)
		Branch left right -> Branch (map_tree left fn) (map_tree right fn)




