module Command where

import Parsing
import Value
import Expr
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
pExpr = do b <- pLogExpr
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
           b <- pBool
           return (Not b)
           ||| do b <- bool
                  return (Val (B b))
           ||| do i <- identifier
                  return (Var i)
           ||| do character '('
                  b <- pLogExpr
                  character ')'
                  return b
           
pLogExpr :: Parser Expr
pLogExpr = do b1 <- pBool
              do symbol "&&"
                 b2 <- pLogExpr
                 return (And b1 b2)
                 ||| do symbol "||"
                        b2 <- pLogExpr
                        return (Or b1 b2)
                        
                 ||| return b1
           

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
