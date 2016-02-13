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
  | Hist AExpr
  | Set  Name  Expr
  | If    BExpr Stmt
  | While BExpr Stmt
  | For Expr BExpr Stmt Stmt
  | Func Name Stmt
  | Exec Name
  deriving (Show, Eq)

data BExpr = Const Bool
  | Not BExpr
  | BVar Expr 
  | Or  BExpr BExpr
  | And BExpr BExpr
  | Eq  AExpr AExpr
  | Gt  AExpr AExpr
  | Lt  AExpr AExpr
  deriving (Show, Eq)

data AExpr = Val Value
  | Neg AExpr
  | AVar Expr
  | Abs AExpr
  | Add AExpr AExpr
  | Sub AExpr AExpr
  | Mul AExpr AExpr
  | Div AExpr AExpr
  | Mod AExpr AExpr
  | Pow AExpr AExpr
  | Fact AExpr
  | Sqrt AExpr
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
                   e <- pAExpr
                   return (Hist e)
            ||| do e <- pExpr
                   return (Eval e)
                  

pExpr :: Parser Expr
pExpr = do b <- pBExpr
           return (Bexp b)
           ||| do a <- pAExpr
                  return (Aexp a)
           
 

pFunc :: Parser Stmt
pFunc = do symbol "func"
           n <- identifier 
           symbol "="
           symbol "{"
           s <- pStmts
           symbol "}"
           return (Func n s)

 
pIf :: Parser Stmt
pIf = do symbol "if"
         symbol "(" 
         b<- pBExpr
         symbol ")"
         symbol "{"
         s <- pStmts
         symbol "}"
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
           symbol "{"
           s <- pStmts
           symbol "}"
           return (For (Var a1) b a2 s)

 
pWhile :: Parser Stmt
pWhile =  do symbol "while"
             symbol "("
             b <- pBExpr
             symbol ")"
             symbol "{"
             s <- pStmts
             symbol "}"
             return (While b s)

pBExpr :: Parser BExpr
pBExpr = do b1 <- pBTerm
            do symbol "&&"
               b2 <- pBExpr
               return (And b1 b2)
               ||| do symbol "||"
                      b2 <- pBExpr
                      return (Or b1 b2)
               ||| return b1
          
                  
            
pBTerm :: Parser BExpr
pBTerm = do b <- bool
            return (Const b)
            ||| do symbol "("
                   e <- pBExpr
                   symbol ")"
                   return e
            ||| do symbol "!"
                   e <-pBExpr
                   return (Not e)
            ||| do v1 <- pAExpr
                   do symbol "<"
                      v2 <- pAExpr
                      return (Lt v1 v2)
                      ||| do symbol "=="
                             v2 <- pAExpr
                             return (Eq v1 v2)
                      ||| do symbol ">"
                             v2 <- pAExpr
                             return (Gt v1 v2)
                      ||| do v <- identifier
                             return (BVar (Var v))

               
pAExpr :: Parser AExpr
pAExpr = do t <- pATerm
            do symbol "+"
               e <- pAExpr
               return (Add t e)
               ||| do symbol "-"
                      e <- pAExpr
                      return (Sub t e)
               ||| return t
            ||| do s <- identifier
                   return (Val (S s))
                   
pATerm :: Parser AExpr
pATerm = do f <- pFactor 
            do symbol "*"
               t <- pATerm
               return (Mul f t)
               ||| do symbol "/"
                      t <- pATerm
                      return (Div f t)
               ||| do symbol "%"
                      t <- pATerm
                      return (Mod f t)
               ||| return f
              
pFactor :: Parser AExpr
pFactor = do f <- float
             return  (Val (F f))
             ||| do i <- integer
                    do symbol "!"
                       return (Fact (Val (I i)))
                       |||return (Val (I i))
             ||| do symbol "-"
                    e <- pFactor
                    return (Neg e)
             ||| do v <- identifier
                    return (AVar (Var v))
             ||| do symbol "("
                    e <- pAExpr
                    symbol ")"
                    return e
             ||| do symbol "|"
                    e <- pAExpr
                    symbol "|"
                    return (Abs e)
             ||| do symbol "Sqrt"
                    symbol "("
                    e <- pAExpr
                    symbol ")"
                    return (Sqrt e)
                          

             
