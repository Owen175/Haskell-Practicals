import Data.Ratio
-- BBP π formula using Rational arithmetic
pie :: Integer -> Rational -> [Rational]
pie n x = newX : pie (n + 1) newX
  where
    newX = x + (1 % (16 ^ n)) *
               (4 % (8 * n + 1)
             - 2 % (8 * n + 4)
             - 1 % (8 * n + 5)
             - 1 % (8 * n + 6))


-- pie :: Int -> Double -> [Double]
-- pie n x = newX : pie (n+1) newX
--     where newX = x+(1/(16^n)) * (4/(8*f+1)-2/(8*f+4)-1/(8*f+5)-1/(8*f+6))
--           f = fromIntegral n