data Tree a = Fork (Tree a) a (Tree a) | Empty

insert :: Ord a => a -> Tree [a] -> Tree [a]
insert a Empty = Fork Empty [a] Empty
insert a (Fork l vals@(v:vs) r) | a > v = Fork l vals (insert a r)  -- insert into right as bigger
                                | a == v = Fork l (a:vals) r  -- insert into the list as equal
                                | otherwise = Fork (insert a l) vals r  -- insert into the left as smaller

flatten :: Tree [a] -> [a]
flatten Empty = []
flatten (Fork l vals r) = flatten l ++ vals ++ flatten r  -- smaller then values then larger

bsort :: Ord a => [a] -> [a]
bsort = flatten . foldr insert Empty  -- given

cp :: [a] -> [[a]]
cp [] = [[]]
cp ls = foldr f [] ls
    where f x xs = [[x,y] | y<-ls] ++ xs

cols :: [[a]] -> [[a]]
cols [] = repeat []
cols (xs:xss) = zipWith (:) xs (cols xss)

cols' :: [[a]] -> [[a]]
cols' = foldr (zipWith (:)) (repeat [])


rjustify :: Int -> String -> String
rjustify n str@(s:ss) | length str > n = rjustify n ss
                      | length str == n = str
                      | otherwise = [' ' | _<-[1..n - length str]] ++ str 

ljustify :: Int -> String -> String
ljustify n str = take n (str ++ repeat ' ')

{-    If the string is wider than the target, 
I take the last n characters for r and first n for l. 
If I wanted to take the whole string, I would add a guard for 
that, returning the full string if string length >= n    -}


type Matrix a = [[a]]

scale :: Num a => a -> Matrix a -> Matrix a
scale scalar as = [map (*scalar) row | row<-as]

dot :: Num a => [a] -> [a] -> a
dot a b = (sum . map product) (cols [a,b])

add :: Num a => Matrix a -> Matrix a -> Matrix a
add a b = [map sum (cols [aRow, bRow]) | (aRow, bRow) <- zip a b]

mul :: Num a => Matrix a -> Matrix a -> Matrix a
mul a b = let bCols = cols b in [[dot row col | col <- bCols] | row<-a]

-- notSameLength is a matrix of strings of the original elements
-- lengthList is a list of the column's maximum lengths
-- lengthSameList is a matrix in which the columns have their length in common
table :: Show a => Matrix a -> String
table matrix = (unlines . map unwords) sameLengthMatrix
    where notSameLength = map (map show) matrix
          lengthList = map (maximum.map length) (cols notSameLength)
          sameLengthMatrix = [[rjustify len x | (len, x) <- zip lengthList row] | row<-notSameLength]