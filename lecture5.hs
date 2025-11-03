f:: (Eq a, Show a) => a -> String
-- This is how you specify dependency on many type classes. 
f x = if x == x then show x else ""

{-
Types of evaluation:

Eager evaluation - calls functions by value
- e.g. square (4+3) -> square (7) -> 7*7 -> 49

Normal order - call by name
- e.g. square (4+3) -> (4+3) * (4+3) -> 7*(4+3) -> 7*7 -> 49
- left most and outermost expanded in each step

Lazy - call only when needed
- e.g. square (3+4) -> let x = 3+4 in x*x -> let x = 7 in x*x -> let x = 7 in 7*x -> let x = 7 in 7*7=49
- doesnt have repeat evaluation like normal order does. 


Normal and lazy evaluation will be guaranteed to find a nomral form if one exists. Eager is not guaranteed
-}

bottom = bottom

true _ = True
true' [] = True
true' (_:_) = True

-- true is not strict, true' is - pattern matching is strict. 
-- bottom: ⊥

-- x = head [4,bottom] - x has a value as bottom is not processed. 

-- recursive definition of concatonation:
[] +++ ys = ys
(x:xs) +++ ys = x:(xs+++ys)

-- non-recursive
xs ++++ ys = [a | as<-[xs,ys], a<-as]


-- takeWhile f ls - takes from a list until the function f is no longer satisfied. 
ls = takeWhile (<10) [1..]

-- fold (right)
-- fold :: (a -> b -> b) -> b -> [a] -> b
-- fold cons nil [] = nil
-- fold cons nil (x:xs) = cons x (fold cons nil xs)

-- essentially takes the head of the list and combines it with the value of fold for the rest of the list
sum' ls = foldr (+) 0 ls

{-
If given a list - [1,2,3,4]
foldr (+) 0 [1,2,3,4]
0
4
7
9
10
return
as foldr (+) 
foldr (+) 0 [1,2,3]
= 1 + (foldr (+) 0 [2,3])
= 1 + (2 + (foldr (+) 0 [3]))
= 1 + (2 + (3 + (foldr (+) 0 [])))
= 1 + (2 + (3 + 0))
= 6
The whole list does not need to be evaluated to get the first n elements either as the rest can remain unevaluated

-}

filter2 p = foldr f []
    where f x xs = if p x then x:xs else xs
-- xs is the result for foldr f [] rest of the list apart from x. 

takeWhile2 p = foldr f [] 
    where f x xs = if not (p x) then [] else x:xs

map2 p = foldr f [] 
    where f x xs = p x : xs