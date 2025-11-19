import Combinators

-- identity - i x = x
i = S:@K:@K

-- combinator - w x = x x
w = S:@i:@i

-- S X Y Z = X Z (Y Z)


omega = w :@ w   -- repeatedly generates w:@w never getting to the rest 


-- need a thing n which makes n a b become a (a(a(a(b)))) for n as
-- can recurse if i have something that converts a b to a(a b)

--a b --> a (a b)



-- generateN 0 = a:@b
-- generateN n = 
--     where (fst:@lst) = generateN (n-1)
-- false :@ S :@ a :@ a :@ b

-- https://www.cantab.net/users/antoni.diller/brackets/intro.html

-- b = S:@(K:@S):@K
-- b' = b:@b
-- c = S:@(b:@b:@S):@(K:@K)
-- c' = b:@(b:@c):@b
-- s' = b:@(b:@(b:@S):@S):@K

-- church numeral successor function:

-- λnfx.f (n f x)

-- lambda x . f(nfx)
-- --> S (lambda x . f) (lambda x . (n f x))
-- --> S (K f) (S (K (n f)) (S K K))
-- then lambda f:
-- --> lambda f . [S (K f) (S (K (n f)) (S K K))]
-- --> S (K S) (S (K K) (S K K)) lhs S (K K) (S K K) rhs 
-- = S (S (K S) (S (K K) (S (K n) (S K K)))) (K (S K K))


-- T[x] = x
-- T[M N] = T[M] T[N]
-- T[λx. M] =
--   K (T[M])             if x ∉ FV(M)
--   I                    if M = x
--   S (T[λx. M1]) (T[λx. M2])   if M = M1 M2

-- lastElem [x] = x
-- lastElem (x:xs) = lastElem xs

-- successor _ n = S :@(S:@ (K:@ S):@ (S:@ (K:@ K):@ (S:@ (K :@ n):@ (S:@ K:@ K)))):@ (K:@ (S:@ K:@ K))

-- getN n = eval (foldr successor (K:@(S:@K:@K)) [1..n])


