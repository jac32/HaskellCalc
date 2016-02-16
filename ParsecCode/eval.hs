module Eval where

import Control.Monad.IO.Class
import System.Console.Haskeline
import CalcState
import CalcParser
import Apply
import BST

{- | Processing of Statements

-}
process :: CalcState -> Stmt -> InputT IO (Either String CalcState)

process st (Stmts []) = return $ Right st
process st (Stmts stmts) =
  do x <- process st (head stmts)
     case x of
       Right st' -> process st' $ Stmts (tail stmts) 
       Left x -> return $ Left x


process st (Assign name expr) =
  case eval st expr of
    Left x  -> return $ Left x
    Right x -> return $ Right $ updateVars name x st
               
process st (If cond stmt1 stmt2) =
  case evalB st cond of
    Left x -> return $ Left x
    Right (B True)  -> process st stmt1
    Right (B False) -> process st stmt2



process st (While cond stmt) =
  case evalB st cond of
    Left x -> return $ Left x
    Right (B True) -> do st' <- process st stmt
                         case st' of
                           Right x -> do st' <- process x (While cond stmt)
                                         return st'
                           Left x -> return $ Left x
    Right (B False) -> return $ Right st


                                    
process st (Func name args stmt) = do let st' = updateFuncs name args stmt st
                                      return $ Right st'

process st (Exec name args) = 
  case (evalList st args) of
    Left x -> return $ Left x
    Right values ->

      case function of 
        Right x -> do st' <- process (genFuncState st x values) (body x) 
                      return st' -- $ updateVars "it" (valOf ) st
                             
        Left error -> return $ Left $  "Function could not be found: \n" ++ error
    
      where function = valOf name (funcs st)

process st (Print expr) =
  case (eval st expr) of
    Left x  -> return $ Left x
    Right x -> do outputStrLn $ show x
                  return $ Right st
                                                     
 
                                      
{- | Evaluation of arithemetic and boolean expressions

-}
evalList :: CalcState -> [Expr] -> Either String [Value]
evalList st (ex:exs) =
  case eval st ex of
    Left error  -> Left $ "Argument could not be evaluated: \n" ++ error
    Right x ->
      case evalList st exs of
        Left error  -> Left $ "Argument could not be evaluated: \n" ++ error
        Right xs -> Right $ x : xs
evalList st [] = Right []
        
  

eval :: CalcState -> Expr -> Either String Value
eval st (Ar expr) = evalA st expr
eval st (Bl expr) = evalB st expr

evalA :: CalcState -> AExpr -> Either String Value
evalA st (Var name)               = valOf name (vars st)
evalA st (NumConst value)         = Right value
evalA st (Neg expr)               = evalA st (ABinary Mul (NumConst (I (-1))) expr)

evalA st (ABinary op expr1 expr2) =
  case (evalA st expr1 , evalA st expr2) of
    (Right x, Right y) ->  apply (AO op) x y
    (Left x, _) -> Left x
    (_, Left y) -> Left y


evalB :: CalcState -> BExpr -> Either String Value
evalB st (BoolConst bool)                       = Right (B bool)
evalB st (Not expr) =
  case evalB st expr of
    Left x -> Left x
    Right (B True) -> Right $ B False
    Right (B False) -> Right $ B True

evalB st (BBinary op expr1 expr2) =
  case (evalB st expr1 , evalB st expr2) of
    (Right x, Right y) ->  apply (BO op) x y
    (Left x, _) -> Left x
    (_, Left y) -> Left y
    
evalB st (RBinary op expr1 expr2) =
  case (evalA st expr1 , evalA st expr2) of
    (Right x, Right y) ->  apply (RO op) x y
    (Left x, _) -> Left x
    (_, Left y) -> Left y
  


 
