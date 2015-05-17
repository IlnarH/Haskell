module Program where

map_via_foldl :: (a -> b) -> [a] -> [b]
map_via_foldl fn a = foldl(\list elem -> list ++ [(fn elem)]) [] a

map_via_foldr :: (a -> b) -> [a] -> [b]
map_via_foldr fn a = foldr (\elem list -> [(fn elem)] ++ list) [] a