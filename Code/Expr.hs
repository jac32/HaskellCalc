module Expr where

import Parsing

type Name = String

data Expr = Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Neg Expr
  | Val Int
  | Var Char
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
  | Eval Expr
  | Fetch Expr
  deriving Show

eval :: [(Name, Int)] ->      -- Variable name to value mapping
        Expr ->               -- Expression to evaluate
        Maybe Int             -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x    -- for values, just give the value directly

--retrieve the value corresponding with the name v (if present) in the vars of the state
--the list comprehension retrieves all name value pairs in vars where the name is equal to v
eval vars (Var v) = Just (snd (head [(name, val) | (name, val) <- vars, name == [v]]))

eval vars (Neg e) = case (eval vars e) of
  (Just x) -> Just (-x)
  _-> Nothing

--performs addition, subtraction, multiplication, and division if 
--eval vars x and eval vars y return a Just int, and not Nothing
eval vars (Add x y) =  case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (x' + y')
  _ -> Nothing

eval vars (Sub x y) =  case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (x' - y')
  _ -> Nothing
  
eval vars (Mult x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (x' * y')
  _ -> Nothing

eval vars (Div x y) = case (eval vars x, eval vars y) of
  (Just x', Just y') -> Just (x' `div` y')
  _ -> Nothing


digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

negDigitToInt :: Char -> Int
negDigitToInt x = fromEnum '0' - fromEnum x

--top of parse tree
pCommand :: Parser Command
pCommand = do char '!'
              e <- pExpr
              return (Fetch e)
              ||| do t <- letter --if variable
                     char '='
                     e <- pExpr
                     return (Set [t] e) --Set t to e, and store in vars in the state
                     ||| do e <- pExpr
                            return (Eval e)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
              ||| do char '-'
                     e <- pExpr
                     return (Sub t e)
              ||| return t


pFactor :: Parser Expr
pFactor = do d <- digit
             return (Val (digitToInt d))
             ||| do char '-' --negative numbers
                    do d <- digit 
                       return (Val(negDigitToInt(d)))
                       ||| do v <- letter --negative variable
                              return (Neg (Var v))
             ||| do v <- letter
                    return (Var v)
                    ||| do char '('
                           e <- pExpr
                           char ')'
                           return e

--gets called in pExpr, to indicate precedence for mult and div               
pTerm :: Parser Expr 
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Mult f t)
              ||| do char '/'
                     t <- pTerm
                     return (Div f t)
              ||| return f

