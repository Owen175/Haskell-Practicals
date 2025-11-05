module Combinators where

import Data.List (unfoldr)


data SKExpr = S | K | SKExpr :@ SKExpr
  deriving (Eq, Show)


combexpr = S :@ (K :@ S) :@ S :@ K

class Reducible a where
  reduce :: a -> Maybe a


-- Exercise 1
instance Reducible SKExpr where
    reduce ((K:@x):@_) = Just x
    reduce (((S:@x):@y):@z) = Just (x :@ z :@ (y :@ z))
    reduce (a:@b) = case reduce a of 
                            Nothing -> Nothing
                            Just x -> Just (x:@b) 
    reduce _ = Nothing



-- Exercise 2
eval :: Reducible a => a -> [a]
eval a = case reduce a of 
    Nothing -> [a]
    Just x -> a:eval x
