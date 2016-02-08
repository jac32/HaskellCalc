module Struct.Statement where

import Parser.Parsing
import Struct.Value

type Name = String

data Stmt = AEval AExpr
  | BEval BExpr
  | ASet  Name  AExpr
  | BSet  Name  BExpr
  | If    BExpr Stmt
  | While BExpr Stmt
  deriving Show

data BExpr = Const Bool
  | Not BExpr
  | BVar Name
  | Or  BExpr BExpr
  | And BExpr BExpr
  | Eq  AExpr AExpr
  | Gt  AExpr AExpr
  | Lt  AExpr AExpr
  deriving Show

data AExpr = Val Value
  | AVar Name
  | Neg AExpr
  | Abs AExpr
  | Add AExpr AExpr
  | Sub AExpr AExpr
  | Mul AExpr AExpr
  | Div AExpr AExpr
  | Mod AExpr AExpr
  | Pow AExpr AExpr
    deriving Show


pStmt :: Parser Stmt
pStmt = do symbol "if"
           b <- pBExpr
           s <- pStmt
           return (If b s)
           ||| do symbol "while"
                  b <- pBExpr
                  s <- pStmt
                  return (While b s)
           ||| do n <- identifier
                  symbol "="
                  v <- pAExpr
                  return (ASet n v)
           ||| do n <- identifier
                  symbol "="
                  v <- pBExpr
                  return (BSet n v)
          ||| do e <- pBExpr
                 return (BEval e)
          ||| do e <- pAExpr
                 return (AEval e)
                  
                 
-- Code here could be simplified
-- NOT needs implemented
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
            ||| do i <- identifier
                   return (BVar i)


               
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
                    return (Val (I i))
             ||| do symbol "-"
                    e <- pFactor
                    return (Neg e)
             ||| do v <- identifier
                    return (AVar v)
             ||| do symbol "("
                    e <- pAExpr
                    symbol ")"
                    return e
             ||| do symbol "|"
                    e <- pAExpr
                    symbol "|"
                    return (Abs e)                 

             
