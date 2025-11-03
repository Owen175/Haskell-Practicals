take_ 0 _ = []
take_ n (x:xs) = x: take (n-1) xs

drop_ 0 xs = xs
drop_ n xs = drop (n-1) xs

evens [] = []
evens [x] = [x]
evens (x:y:xs) = x: evens xs

odds [] = []
odds [x] = []
odds (x:y:xs) = y:odds xs

alts [] = ([], [])
alts [x] = ([x], [])
alts (x:y:xs) = (x:prevEvens, y:prevOdds)
    where (prevEvens, prevOdds) = alts(xs)

permutations' ls = foldr include [[]] ls

-- include gets something like 1 [[]] and needs to produce [[1]]
-- it could get 1 [[2]] and need to make [[1,2], [2,1]]
-- therefore - need to take each element of the list and put the x inbetween

-- include x ls = [take idx list ++ (x:drop idx list) | list<-ls, idx<-[0..length list]]
include x ls = foldr f [[x]] ls
    where f = 
-- include x ls = [x:a | a<-ls]

-- include x ls = foldr f [[x]] ls

--     where f :: a->[[a]] -> [[a]]
--           f y rest = [take a oneRest ++ (y:(drop a oneRest)) | oneRest <- rest, a<-[0..length oneRest]]