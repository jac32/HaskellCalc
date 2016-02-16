module Eval where

import Control.Monad.IO.Class
import Paths_HaskellCalc
import Text.ParserCombinators.Parsec
import System.Console.Haskeline
import CalcState
import CalcParser
import Apply
import BST

-- |The following function takes a state and input from the user or from a file,
-- and calls process on it so that the statements are appropriately evaluated
parseStatement :: CalcState -> String -> InputT IO (Either String CalcState)
parseStatement st input = 
  case parse statement "" input of
    Right stmt -> do x <- process st stmt
                     case x of
                       Right st'  -> do let st'' = addHistory st' stmt
                                        return $ Right st''
                       Left x -> return $ Left x
                       
    Left error -> do outputStrLn $ show error
                     return $ Right st



{- | Processing of Statements using pattern matching
-}
process :: CalcState -> Stmt -> InputT IO (Either String CalcState)

process st Quit = return $ Left "exit"
process st Help = do path <- liftIO $ getDataFileName "README.md"
                     contents <- liftIO $ readFile path
                     outputStrLn contents
                     return $ Right st
                     

process st (Load file) = do contents <- liftIO $ readFile file
                            do x <- parseStatement st contents 
                               case x of
                                 Right st -> return $ Right st 
                                 Left error -> return $ Left error
  

-- | processes a list of statements by calling process on
-- the head, and subsequently on the tail until the entire list has 
-- been processed
process st (Stmts []) = return $ Right st
process st (Stmts stmts) =
  do x <- process st (head stmts)
     case x of
       Right st' -> process st' $ Stmts (tail stmts) 
       Left x -> return $ Left x

-- | This process the Assign statement and adds the new var to the tree 
-- by calling updateVars
process st (Assign name expr) =
  case eval st expr of
    Left x  -> return $ Left x
    Right x -> return $ Right $ updateVars name x st


-- | Processing a if loop by evaluating its condition. If true, the 
-- statement is evaluated              
process st (If cond stmt1 stmt2) =
  case evalB st cond of
    Left x -> return $ Left x
    Right (B True)  -> process st stmt1
    Right (B False) -> process st stmt2


-- | Processing a while loop by evaluating its statement and subsequently
-- recursively calls the function until the cond evaluates to false
process st (While cond stmt) =
  case evalB st cond of
    Left x -> return $ Left x
    Right (B True) -> do st' <- process st stmt
                         case st' of
                           Right x -> do st' <- process x (While cond stmt)
                                         return st'
                           Left x -> return $ Left x
    Right (B False) -> return $ Right st


-- |Processes a new function by calling the updateFuncs function, which adds it to 
-- the BST for functions for the given state                                    
process st (Func name args stmt) = do let st' = updateFuncs name args stmt st
                                      return $ Right st'

-- | Process a file exec statement by evaluating the list of arguments. A new state is 
-- created for the function using the genFuncState function (to simulate local scope),
-- and then the function's statements are executed.
process st (Exec name args) = 
  case (evalList st args) of
    Left x -> return $ Left x
    Right values ->

      case function of 
        Right x -> do st' <- process (genFuncState st x values) (body x) 
                      return $ Right st
        Left error -> return $ Left $  "Function could not be found: \n" ++ error
    
      where function = valOf name (funcs st)

-- | This function prints out the evaluated expression, if valid, and then returns a new state
process st (Print expr) =
  case (eval st expr) of
    Left x  -> return $ Left x
    Right x -> do outputStrLn $ show x
                  return $ Right st
                                                     
-- | This function evaluates a history command by fetching the nth element
-- from the list of history statements from the provided CalcState
process st (Hist arg) = case fetchHistory st arg of
  Right x -> process st x
  Left x -> return $ Left $ "History " ++ x ++ ": " ++ (show arg)
                                      

{- Evaluation of arithemetic and boolean expressions
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

-- |This generic eval function calls the corresponding eval function for
-- the given expression type through pattern-matching         
eval :: CalcState -> Expr -> Either String Value
eval st (Ar expr) = evalA st expr
eval st (Bl expr) = evalB st expr

-- |evaluates arithmetic expressions through pattern matching
evalA :: CalcState -> AExpr -> Either String Value
evalA st (Var name)               = valOf name (vars st)
evalA st (NumConst value)         = Right value
evalA st (Neg expr)               = evalA st (ABinary Mul (NumConst (I (-1))) expr)

evalA st (ABinary op expr1 expr2) =
  case (evalA st expr1 , evalA st expr2) of
    (Right x, Right y) ->  apply (AO op) x y
    (Left x, _) -> Left x
    (_, Left y) -> Left y


-- |evaluates boolean  expressions through pattern matching
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
  


 
