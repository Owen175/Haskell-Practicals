module Combinators where

import Data.List (unfoldr)


data SKExpr = S | K | SKExpr :@ SKExpr
  deriving (Eq, Show)

combexpr = S :@ (K :@ S) :@ S :@ K

class Reducible a where
  reduce :: a -> Maybe a
  reduceAll :: a -> [a]

-- Exercise 1
instance Reducible SKExpr where
    reduce ((K:@x):@_) = Just x
    reduce (((S:@x):@y):@z) = Just (x :@ z :@ (y :@ z))
    reduce (a:@b) = case reduce a of 
                            Nothing -> case reduce b of 
                              Nothing -> Nothing
                              Just x -> Just (a:@x)
                            Just x -> Just (x:@b) 
    reduce _ = Nothing

    reduceAll ((K:@x):@y) = x: [K:@ ra :@ y | ra<-reduceAll x] ++ [K:@x:@ra | ra<-reduceAll y]
    reduceAll (((S:@x):@y):@z) = x :@ z :@ (y :@ z) : [S:@ ra :@ y :@ z | ra<-reduceAll x] ++ [S:@ x :@ ra :@ z | ra<-reduceAll y] ++ [S:@ x :@ y :@ ra | ra<-reduceAll z] 
    reduceAll (a:@b) = [ra :@ b | ra<-reduceAll a] ++ [a:@ra | ra<-reduceAll b]
    reduceAll _ = []
    




-- Exercise 2
eval :: Reducible a => a -> [a]
eval a = case reduce a of 
    Nothing -> [a]
    Just x -> a:eval x

data Tree a = Node [Tree a] a


evalTree a = Node skls a
    where skls = map evalTree (reduceAll a)
 
flattenEvalTree (Node [] a) = [[a]]
flattenEvalTree (Node xs c) = map (c:) (concat [flattenEvalTree x | x<-xs])

evalAll :: Reducible a => a -> [[a]]
evalAll a = (flattenEvalTree . evalTree) a


-- evalAll :: Reducible a => a -> [[a]]
-- evalAll --

-- reduceAll (Just unchanged) ((K:@x):@y) = x: reduceAll (Just (unchanged :@ K)) (x:@y)
-- reduceAll Nothing ((K:@x):@y) = x: reduceAll (Just K) (x:@y)
-- reduceAll (Just unchanged) (((S:@x):@y):@z) = (unchanged :@ x :@ z :@ (y :@ z)) : reduceAll (Just (unchanged :@ S)) (x:@y:@z)
-- reduceAll Nothing (((S:@x):@y):@z) = (x :@ z :@ (y :@ z)) : reduceAll (Just S) (x:@y:@z)

-- reduceAll (Just unchanged) (a:@b) = [t:@b | t<-reduceAll (Just unchanged) a] ++ [t | t<-reduceAll (Just (a:@unchanged)) b]
-- reduceAll Nothing (a:@b) = [t:@b | t<-reduceAll Nothing a] ++ [t | t<-reduceAll (Just a) b]
-- reduceAll Nothing a = [a]
-- reduceAll (Just unchanged) a = [unchanged :@ a]