module Bools where

import Prelude hiding (True , False)
import Combinators


-- Exercise 3
true , false :: SKExpr
true = K
false = S:@K

data BoolExpr = True | False | Or BoolExpr BoolExpr | And BoolExpr BoolExpr | Neg BoolExpr
  deriving (Eq, Show)

boolexpr = True `And` (True `And` Neg (False `Or` True))

-- Exercise 4
compileBool :: BoolExpr -> SKExpr
compileBool True = true
compileBool False = false
compileBool (And a b) = aCompiled:@K:@false:@(bCompiled):@false
    where aCompiled = compileBool a
          bCompiled = compileBool b
compileBool (Or a b) = compileBool (Neg (And (Neg a) (Neg b)))
    where aCompiled = compileBool a
          bCompiled = compileBool b
compileBool (Neg a) = aCompiled :@ false :@ true
    where aCompiled = compileBool a

expand (a) = a

evalBool :: BoolExpr -> IO ()
evalBool bexpr = case last (eval (compileBool bexpr)) of
  b | b == true -> print (show bexpr ++ " is True")
  b | b == false -> print (show bexpr ++ " is False")
  _ -> print "Didn't reduce to a Bool"