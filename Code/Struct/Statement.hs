module Struct.Statement where

import Parser.Parsing
import Struct.Value

type Name = String

data Expr = Aexp AExpr
  | Bexp BExpr
  deriving (Show, Eq)

data Stmt = Stmts Stmt Stmt
  | Eval Expr
  | Hist AExpr
  | Set  Name  Expr
  | If    BExpr Stmt
  | While BExpr Stmt
  | For AExpr BExpr Stmt Stmt
  | Func Name Stmt
  | Exec Name
  deriving (Show, Eq)

data BExpr = Const Bool
  | Not BExpr
  | BVar Name 
  | Or  BExpr BExpr
  | And BExpr BExpr
  | Eq  AExpr AExpr
  | Gt  AExpr AExpr
  | Lt  AExpr AExpr
  deriving (Show, Eq)

data AExpr = Val Value
  | Neg AExpr
  | AVar Name
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
            ||| do s <- pSet
                   return s
            ||| do e <- pExec
                   return e
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
                  
                  
pSet :: Parser Stmt
pSet =  do n <- identifier
           symbol "="
           v <- pExpr
           return (Set n v)
                  
                  
pExec :: Parser Stmt
pExec = do n<- identifier
           symbol "()"
           return (Exec n)

pFunc :: Parser Stmt
pFunc = do symbol "func"
           n <- identifier 
           s <- pBody 
           return (Func n s)

 
pIf :: Parser Stmt
pIf = do symbol "if"
         b <- pBBrac
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
           return (For (AVar a1) b a2 s)

 
pWhile :: Parser Stmt
pWhile =  do symbol "while"
             b <- pBBrac 
             s <- pBody
             return (While b s)

pBExpr :: Parser BExpr
pBExpr = do b <- pBTerm
            do e <- pAnd b
               return e
               ||| do e <- pOr b
                      return e
                      ||| return b
          
                  
            
pBTerm :: Parser BExpr
pBTerm = do b <- bool
            return (Const b)
            ||| do e <- pBBrac
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
                      ||| do v <- symbol "it"
                             return (BVar v)
                      ||| do v <- identifier
                             return (BVar v)

               
pAExpr :: Parser AExpr
pAExpr = do t <- pATerm
            do e <- (pLowPrec t)
               return e


pLowPrec :: AExpr -> Parser AExpr 
pLowPrec t = do e <- pAdd t
                return e
                ||| do e <- pSub t
                       return e
                ||| return t


pATerm :: Parser AExpr
pATerm = do f <- pFactor 
            do e <- pOpPrec f
               return e

pOpPrec :: AExpr -> Parser AExpr
pOpPrec f = do e <- pDiv f 
               return e
               ||| do e <- pMul f
                      return e
               ||| do e <- pMod f
                      return e
               ||| return f 
              
              
pFactor :: Parser AExpr
pFactor = do f <- float
             do e <- pPow (Val (F f))
                return e
                ||| return  (Val (F f))
             ||| do i <- integer
                    do e <-pFact i
                       return e
                       ||| do e <- pPow (Val (I i))
                              return e
                       |||return (Val (I i))
             ||| do e <- pNeg
                    return e 
             ||| do v <- symbol "it"
                    return (AVar v)
             ||| do v <- identifier
                    return (AVar v)
             ||| do e <- pBrac
                    return e
             ||| do e <- pAbs
                    return e 
             ||| do symbol "Sqrt"
                    e <- pBrac 
                    return (Sqrt e)


pPow :: AExpr -> Parser AExpr
pPow f = do symbol "^"
            t <- pFactor
            do e <- pOpPrec (Pow f t)
               return e
               ||| return (Pow f t)

pMod :: AExpr -> Parser AExpr
pMod f = do symbol "/"
            t <- pFactor
            do e <- pOpPrec (Mod f t)
               return e
               ||| return (Mod f t)

pDiv :: AExpr -> Parser AExpr
pDiv f = do symbol "/"
            t <- pFactor
            do e <- pOpPrec (Div f t)
               return e
               ||| return (Div f t)
 
pMul :: AExpr -> Parser AExpr
pMul f = do symbol "*"
            t <- pFactor
            do e <- pOpPrec (Mul f t)
               return e
               ||| return (Mul f t)
          
pNeg :: Parser AExpr
pNeg = do symbol "-"
          e <- pFactor
          return (Neg e)

pAdd :: AExpr -> Parser AExpr
pAdd t = do symbol "+"
            e <- pAExpr
            do f <- (pLowPrec (Add t e))
               return f
               ||| return (Add t e)

pSub :: AExpr -> Parser AExpr
pSub t = do symbol "-"
            e <- pAExpr
            do f <- (pLowPrec (Sub t e))
               return f
               ||| return (Sub t e)

pAnd :: BExpr -> Parser BExpr 
pAnd b1 = do symbol "&&"
             b2 <- pBExpr
             return (And b1 b2)

pOr :: BExpr -> Parser BExpr 
pOr b1 = do symbol "&&"
            b2 <- pBExpr
            return (Or b1 b2)


pBody :: Parser Stmt
pBody = do symbol "{"
           s <- pStmts
           symbol "}"
           return s



pBBrac :: Parser BExpr 
pBBrac = do symbol "("
            e <- pBExpr
            symbol ")"
            return e

pBrac :: Parser AExpr 
pBrac = do symbol "("
           e <- pAExpr
           symbol ")"
           return e


pAbs :: Parser AExpr 
pAbs = do symbol "|"
          e <- pAExpr
          symbol "|"
          return (Abs e)

pFact :: Int -> Parser AExpr
pFact i = do symbol "!"
             return (Fact (Val (I i)))
