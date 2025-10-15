{-
Ex. 1: factor 0 = 
factorFrom 2 0 
q r = 0 divMod 2
q = 0 r = 0
therefore returns (2, 0)

factor 1 = 
factorFrom 2 1 
q r = 1 divMod 2
q = 0
r = 1
therefore factorFrom 3 1
q r = 1 divmod 3
q = 0 r = 3
This will continue increasing m but not changing n. Therefore, this will just loop forever. No return
Ex. 2: completed

Ex. 3
The smallest factor of n (a) must multiply with another integer to make n. If this number > root n
then the other number in the factor pair (b) must be n/a, which is smaller than root n, and therefore smaller than a. 
Therefore, b will be a smaller factor than a, causing a contradiction. 

The order DOES matter - if the check for n<=m*m comes before the r==0, then a square number with no other 
factors, such as 49, will return no factors, when it has 7*7

The worst case recursive calls will occur in this case, with approximately root n calls.


Ex. 4
The purpose of n <= m * m is to ensure than if m >= root n you exit, showing no factors. 
q is n div m. Therefore, if q < m, this means that m > root n. This means that there will be no factors of this size or larger 
as previously established. 

This is more efficient as it requires fewer operations - q<m is less costly to compute than n<=m*m

Ex.5:
I would expect this to use around half of the recursive calls, since it is incrementing m by 2 rather than 1
each call. Therefore, twice as efficient.

Ex. 6:
- Test Results: applying factor3 to 2-100. 
- [(2,1),(3,1),(2,2),(-1,-1),(2,3),(-1,-1),(2,4),(3,3),(2,5),(-1,-1),(2,6),(-1,-1),(2,7),(3,5),(2,8),(-1,-1),(2,9),(-1,-1),(2,10),(3,7),(2,11),(-1,-1),(2,12),(5,5),(2,13),(3,9),(2,14),(-1,-1),(2,15),(-1,-1),(2,16),(3,11),(2,17),(5,7),(2,18),(-1,-1),(2,19),(3,13),(2,20),(-1,-1),(2,21),(-1,-1),(2,22),(3,15),(2,23),(-1,-1),(2,24),(7,7),(2,25),(3,17),(2,26),(-1,-1),(2,27),(5,11),(2,28),(3,19),(2,29),(-1,-1),(2,30),(-1,-1),(2,31),(3,21),(2,32),(5,13),(2,33),(-1,-1),(2,34),(3,23),(2,35),(-1,-1),(2,36),(-1,-1),(2,37),(3,25),(2,38),(7,11),(2,39),(-1,-1),(2,40),(3,27),(2,41),(-1,-1),(2,42),(5,17),(2,43),(3,29),(2,44),(-1,-1),(2,45),(7,13),(2,46),(3,31),(2,47),(5,19),(2,48),(-1,-1),(2,49),(3,33),(2,50)]
- (0.01 secs, 753,272 bytes)

2-10,000 for factor3: (0.21 secs, 106,425,704 bytes)
2-10,000 for factor2: (0.26 secs, 136,161,944 bytes)

Not really twice as fast sadly. It is faster and uses less space however. 

Ex. 7:
2-10: [(2,1),(3,1),(2,2),(-1,-1),(2,3),(-1,-1),(2,4),(3,3),(2,5)]
Correct answers. 
To 10,000:
(0.21 secs, 108,033,464 bytes) - essentially 0 difference. 

Ex. 8:
It may be more costly to generate the prime list than the cost saved by using it. 

Ex.9:
Works - had to make a few modifications to ensure that s was correct and m was odd. 
Test data:
[[2],[3],[2,2],[5],[2,3],[7],[2,2,2],[3,3],[2,5],[11],[2,2,3],[13],[2,7],[3,5],[2,2,2,2],[17],[2,3,3],[19],[2,2,5],[3,7],[2,11],[23],[2,2,2,3],[5,5]]
correct. 

Ex.10:
(0.07 secs, 46,958,088 bytes)
vs
(0.06 secs, 31,881,440 bytes)
Therefore, factors2 uses significantly less memory and a bit less time. 

As you come across larger numbers, factors is significantly slower and less space efficient than
factors2, as shown below. 
(7.11 secs, 5,043,200,200 bytes) - factors mapped from 2-10,000
(0.45 secs, 245,557,824 bytes) - factors2 mapped from 2-10,000
-}
-- given
factor :: Integer -> (Integer, Integer)
factor n = factorFrom 2 n

factorFrom :: Integer -> Integer -> (Integer, Integer)
factorFrom m n | r == 0 = (m, q)
               | otherwise = factorFrom (m+1) n
    where (q, r) = divMod n m


factor1 :: Integer -> (Integer, Integer)
factor1 n = factorFrom1 2 n

factorFrom1 :: Integer -> Integer -> (Integer, Integer)
factorFrom1 m n | r == 0 = (m, q)
                | n <= m*m = (-1, -1)  -- no factors
                | otherwise = factorFrom1 (m+1) n
    where (q, r) = divMod n m


factor2 :: Integer -> (Integer, Integer)
factor2 n = factorFrom2 2 n 

factorFrom2 :: Integer -> Integer -> (Integer, Integer)
factorFrom2 m n | r == 0 = (m, q)
                | q<m = (-1, -1)  -- no factors - more efficient
                | otherwise = factorFrom2 (m+1) n
    where (q, r) = divMod n m


factor3 :: Integer -> (Integer, Integer)
factor3 n | mod n 2 == 0 = (2, div n 2)  -- filters the 2s out so that factorFrom3 is cleanly odd
          | otherwise = factorFrom3 3 n 

factorFrom3 :: Integer -> Integer -> (Integer, Integer)
factorFrom3 m n | r == 0 = (m, q)
                | q<m = (-1, -1)  -- no factors
                | otherwise = factorFrom3 (m+2) n
    where (q, r) = divMod n m


factor4 :: Integer -> (Integer, Integer)
factor4 n = factorFrom4 3 n 2

factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
factorFrom4 m n s | mod n 2 == 0 = (2, div n 2)
                  | r == 0 = (m, q)
                  | q<m = (-1, -1)  -- no factors
                  | otherwise = if m == 3 then factorFrom4 (m+s) n s
                                else factorFrom4 (m+s) n (6-s)
    where (q, r) = divMod n m



-- given
factors :: Integer -> [Integer]
factors n = factorsFrom 2 n

factorsFrom :: Integer -> Integer -> [Integer]
factorsFrom m n | n == 1    = []
                | otherwise = p:factorsFrom p q
    where (p,q) = factorFrom m n

factors2 :: Integer -> [Integer]
factors2 n = factorsFrom2 3 n

factorsFrom2 :: Integer -> Integer -> [Integer]
factorsFrom2 m n | n == 1    = []
                 | (p, q) == (-1, -1) = [n] -- if no factors, put number itself.
                 | otherwise = p:factorsFrom2 p q
    where (p, q) = factorFrom4 newM n newS
          newM = (m - (mod m 2) + 1) -- makes odd (required for factorFrom4)
          newS = if mod newM 3 == 1 then 4 else 2 -- sets s so sols aren't skipped
