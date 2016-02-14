module Struct.Statement where

import Parser.Parsing
import Struct.Value

type Name = String

data Expr = Aexp AExpr
  | Bexp BExpr
  | Var Name
  deriving (Show, Eq)

data Stmt = Stmts Stmt Stmt
  | Eval Expr
  | Hist Expr
  | Set  Name  Expr
  | If    Expr Stmt
  | While Expr Stmt
  | For Expr Expr Stmt Stmt
  | Func Name Stmt
  | Exec Name
  deriving (Show, Eq)

data BExpr = Const Bool
  | Not Expr 
  | Or  Expr Expr
  | And Expr Expr
  | Eq  Expr Expr
  | Gt  Expr Expr
  | Lt  Expr Expr
  deriving (Show, Eq)

data AExpr = Val Value
  | Neg Expr
  | Abs Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Pow Expr Expr
  | Fact Expr
  | Sqrt Expr
    deriving (Show, Eq)


pStmts :: Parser Stmt
pStmts = do s1 <- pStmt
            do symbol ";"
               s2 <- pStmts
               return (Stmts s1 s2)
               ||| return s1
            
       

pStmt :: Parser Stmt
pStmt =  do i <- pIf
            return i
            ||| do f <- pFunc
                   return f
            ||| do f <- pFor
                   return f          
            ||| do w <- pWhile
                   return w
            ||| do n <- identifier
                   symbol "="
                   v <- pExpr
                   return (Set n v)
            ||| do n<- identifier
                   symbol "()"
                   return (Exec n)
            ||| do symbol "$"
                   e <- pExpr
                   return (Hist e)
            ||| do e <- pExpr
                   return (Eval e)
                  

pExpr :: Parser Expr
pExpr = do b <- pBExpr
           return b
           ||| do a <- pAExpr
                  return a
           
 

pFunc :: Parser Stmt
pFunc = do symbol "func"
           n <- identifier 
           s <- pBody 
           return (Func n s)

 
pIf :: Parser Stmt
pIf = do symbol "if"
         b <- pBrac
         s <- pBody
         return (If b s)

  
pFor :: Parser Stmt
pFor =  do symbol "for"
           symbol "("
           a1 <- identifier
           symbol ";"
           b <- pBExpr
           symbol ";"
           a2 <- pStmt
           symbol ")"
           s <- pBody           
           return (For (Var a1) b a2 s)

 
pWhile :: Parser Stmt
pWhile =  do symbol "while"
             b <- pBrac
             s <- pBody
             return (While b s)

          
pBExpr :: Parser Expr
pBExpr = do a <- pAnd
            return a
            ||| do a <- pOr
                   return a 
                  
            
pBTerm :: Parser Expr
pBTerm = do b <- bool
            return (Bexp (Const b))
            ||| do e <- pBrac
                   return e
            ||| do e <- pNot
                   return e
            ||| do e <- pLt
                   return e
                   ||| do e <- pEq
                          return e
                   ||| do e <- pGt
                          return e 
                   ||| do v <- identifier
                          return (Var v)

               
pAExpr :: Parser Expr
pAExpr = do t <- pATerm
            do e <- pAdd
               return (Aexp (Add t e))
               ||| do e <- pSub
                      return (Aexp (Sub t e))
               ||| return t
           -- ||| do s <- identifier
             --      return (Val (S s))
                   
pATerm :: Parser Expr
pATerm = do f <- pFactor 
            do t <- pPow
               return (Aexp (Pow f t)) 
               ||| do t <- pMul
                      return (Aexp (Mul f t))
               ||| do t <- pDiv
                      return (Aexp (Div f t))
               ||| do t <- pMod
                      return (Aexp (Mod f t))
               ||| return f
             
 
pFactor :: Parser Expr
pFactor = do f <- float
             return  (Aexp (Val (F f)))
             ||| do i <- pInt
                    return i
             ||| do e <- pNeg
                    return e
             ||| do v <- identifier
                    return (Var v)
             ||| do e <- pBrac
                    return e
             ||| do e <- pAbs
                    return e
             ||| do e <- pSqrt
                    return e 


pBody :: Parser Stmt
pBody = do symbol "{"
           s <- pStmts
           symbol "}"
           return s


pOr :: Parser Expr
pOr = do b1 <- pBTerm
         do symbol "||"
            b2 <- pBExpr
            return (Bexp (Or b1 b2))

pAnd :: Parser Expr
pAnd = do b1 <- pBTerm
          do symbol "&&"
             b2 <- pBExpr
             return (Bexp (And b1 b2))

pGt :: Parser Expr
pGt = do v1 <- pExpr
         do symbol ">"
            v2 <- pExpr
            return (Bexp (Eq v1 v2))

pEq :: Parser Expr
pEq = do v1 <- pExpr
         do symbol "=="
            v2 <- pExpr
            return (Bexp (Eq v1 v2))

pLt :: Parser Expr
pLt = do v1 <- pExpr
         do symbol "<"
            v2 <- pExpr
            return (Bexp (Lt v1 v2))


pNot :: Parser Expr
pNot = do symbol "!"
          e <-pExpr
          return (Bexp (Not e))

pAdd :: Parser Expr
pAdd = do symbol "+"
          e <- pExpr
          return e

pSub :: Parser Expr
pSub = do symbol "-"
          e <- pExpr
          return e

pInt :: Parser Expr
pInt = do i <- integer
          do symbol "!"
             return (Aexp (Fact (Aexp (Val (I i)))))
             |||return (Aexp (Val (I i)))
                  
pSqrt :: Parser Expr
pSqrt = do symbol "Sqrt"
           e <- pBrac
           return (Aexp (Sqrt e))

pAbs :: Parser Expr
pAbs = do symbol "|"
          e <- pExpr
          symbol "|"
          return (Aexp (Abs e))

pNeg :: Parser Expr
pNeg = do symbol "-"
          e <- pFactor
          return (Aexp (Neg e))       
               
pBrac :: Parser Expr
pBrac = do symbol "("
           e <- pExpr
           symbol ")"
           return e

pPow :: Parser Expr
pPow =  do symbol "^"
           t <- pATerm
           return t           

pDiv :: Parser Expr
pDiv =  do symbol "/"
           t <- pATerm
           return t 

pMul :: Parser Expr
pMul =  do symbol "*"
           t <- pATerm
           return t
 
pMod :: Parser Expr
pMod =  do symbol "%"
           t <- pATerm
           return t 
