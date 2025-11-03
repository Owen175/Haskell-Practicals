zip2 :: [a] -> [b] -> [(a, b)]
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys

permutations2 :: Eq a => [a] -> [[a]]
permutations2 [] = [[]]
permutations2 xs = [x:ys | x<-xs, ys<-permutations2 (delete x xs)]

delete :: Eq a => a -> [a] -> [a]
delete x xs = [a | a<-xs, a/=x]

-- uncurry (<=) is the same as f (x,y) = x<=y

insertion :: (Foldable t, Ord a) => t a -> [a]
insertion = foldr insert []

insert :: Ord t => t -> [t] -> [t]
insert x [] = [x]
insert x (y:ys) = if x>y then y:insert x ys else x:y:ys

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<x) xs) ++ (x:quicksort (filter (>=x) xs))

--takewhile condition ls 
--dropwhile condition ls - takes from the first time it fails until the end of the list - not inclusive

unfold2 :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold2 n h t x = if n x then [] else h x : unfold2 n h t (t x)


{- 
Unfolds:
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold n h t x = if n x then [] else h x : unfold n h t (t x)
first arg is a function which tells you if the outcome should be []
second arg is a function which converts from type t to type a to get the head of the list. 
third arg is a function which goes from t to t. Finally, you input the first input

This repeatedly applies t to x, applying h to the result and outputting each. 
-}

-- e.g.
doubling :: Double -> [Double]
doubling = unfold2 (>2^20) (\x->x) (2*)

msort [] = []
msort [x] = [x]
msort xs = merge (msort ls) (msort rs)
    where (ls, rs) = halve xs

merge (x:xs) (y:ys) | x<=y = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
merge [] ys = ys
merge xs [] = xs


halve xs = (take n xs, drop n xs) where n = length xs `div` 2