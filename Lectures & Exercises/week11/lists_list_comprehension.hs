import Prelude

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x      = x : filter' p xs
    |otherwise = filter' p xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs

elem' :: Eq a => a -> [a] -> [a]
elem' _ [] = []
elem' e (x:xs)
    | e == x    = x:xs
    | otherwise = elem' e xs

reverseFoldr lst = foldr (\ el res -> res ++ [el]) [] lst
reverseFoldl lst = foldl (\res el ->el : res) [] lst




minimum' (x:xs) = foldl min x xs
maximum' (x:xs) = foldr max x xs

lengthFoldl :: [a] -> Int
lengthFoldl xs = foldl (\res _ -> res+1) 0 xs
