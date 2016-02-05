module Expr where

import Parsing
import Value
import BST

type Name = String

data Expr = Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Neg Expr
  | Abs Expr
  | Pow Expr Expr
  | Mod Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Val Value
  | Var Name
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
  | Eval Expr
  | Fetch Expr
  deriving Show

toValue :: Maybe Value -> Value
toValue (Just x) = x

eval :: Tree (Name,Value) ->      -- Variable name to value mapping
        Expr ->               -- Expression to evaluate
        Maybe Value             -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x    -- for values, just give the value directly

--retrieve the value corresponding with the name v (if present) in the vars of the state
--the list comprehension retrieves all name value pairs in vars where the name is equal to v
eval vars (Var v) = Just (valOf v vars)

eval vars (Neg e) = case (eval vars e) of
  (Just x) -> Just (mulV (toValue((eval vars (Val (I (-1)))))) (x))
  _-> Nothing

--performs absolute value, power, and modulo if eval vars x and 
--eval vars y both return Just Ints, and not Nothing
eval vars (Abs e) = case (eval vars e) of
  (Just x) -> Just (absV x) 
  _ -> Nothing

eval vars (Pow x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (powV x' y')
  _ -> Nothing

eval vars (Mod x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (modV x' y')
  _ -> Nothing


--performs addition, subtraction, multiplication, and division if 
--eval vars x and eval vars y return a Just int, and not Nothing
eval vars (Add x y) =  case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just(addV (x') (y')) 
  _ -> Nothing

eval vars (Sub x y) =  case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (subV (x') (y'))
  _ -> Nothing
  
eval vars (Mult x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (mulV (x') (y'))
  _ -> Nothing

eval vars (Div x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (divV (x') (y'))
  _ -> Nothing

--- Bool operation evaluations
eval vars (Not e) = case (eval vars e) of
  (Just x) -> Just (notV x) 
  _ -> Nothing

eval vars (And x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (andV x' y')
  _ -> Nothing

eval vars (Or x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (orV x' y')
  _ -> Nothing

  
--top of parse tree
pCommand :: Parser Command
pCommand = do character '#'
              e <- pExpr
              return (Fetch e)
              ||| do t <- identifier --if variable
                     character '='
                     e <- pExpr
                     return (Set t e) --Set t to e, and store in vars in the state
                     ||| do e <- pExpr
                            return (Eval e)

pExpr :: Parser Expr
pExpr = do b <- pBool
           return b
           ||| do t <- pTerm
                  do character '+'
                     e <- pExpr
                     return (Add t e)
                     ||| do character '-'
                            e <- pExpr
                            return (Sub t e)
                     ||| return t


pBool :: Parser Expr
pBool = do symbol "!"
           b <- bool
           return (Not (Val(B b)))
           ||| do b <- bool
                  return (Val (B b))
           








-- pBool :: Parser Expr
-- pBool = do b <- bool
--            return (Val (B b))
           

pFactor :: Parser Expr
pFactor = do d <- float
             return (Val  (F d))
             ||| do d <- integer
                    return (Val (I d))
             ||| do character '-'
                    e <- pFactor --negative expressions
                    return (Neg (e))
             ||| do v <- identifier
                    return (Var v)
                    ||| do character '('
                           e <- pExpr
                           character ')'
                           return e
                    ||| do character '|'
                           e <- pExpr
                           character '|'
                           return (Abs e)
--gets called in pExpr, to indicate precedence for mult and div               
pTerm :: Parser Expr 
pTerm = do f <- pFactor
           do character '^'
              t <- pExpr
              return (Pow f t)
              ||| do character '*'
                     t <- pTerm
                     return (Mult f t)
              ||| do character '/'
                     t <- pTerm
                     return (Div f t)
              ||| do character '%'
                     t <- pTerm
                     return (Mod f t)
              ||| return f

