module Program where

filter_via_recursion :: (a -> Bool) -> [a] -> [a]
filter_via_recursion fn [] = []
filter_via_recursion fn (x:xs) = if fn x
		then x : (filter_via_recursion fn xs)
		else (filter_via_recursion fn xs)

filter_via_foldl :: (a -> Bool) -> [a] -> [a]
filter_via_foldl fn a = foldl (\list elem -> if fn elem 
													then list ++ [elem] 
											else list) [] a

filter_via_foldr :: (a -> Bool) -> [a] -> [a]
filter_via_foldr fn a = foldr (\elem list -> if fn elem 
													then elem : list 
											else list) [] a

concat_via_recursion :: [[a]] -> [a]
concat_via_recursion [] = []
concat_via_recursion (x:xs) = x ++ concat_via_recursion xs

concat_via_foldl :: [[a]] -> [a]
concat_via_foldl ar = foldl (++) [] ar

concat_via_foldr :: [[a]] -> [a]
concat_via_foldr ar = foldr (++) [] ar

concatMap_via_recursion :: (a -> [b]) -> [a] -> [b]
concatMap_via_recursion fn [] = []
concatMap_via_recursion fn (x:xs) =  (++) (fn x) (concatMap_via_recursion fn xs)

concatMap_via_foldl :: (a -> [b]) -> [a] -> [b]
concatMap_via_foldl fn a = foldl (\list elem -> list ++ fn elem) [] a

concatMap_via_foldr :: (a -> [b]) -> [a] -> [b]
concatMap_via_foldr fn a = foldr (\elem list -> fn elem ++ list) [] a
