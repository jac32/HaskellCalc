module REPL where

import Control.Monad.IO.Class
import System.Console.Haskeline
import Struct.Statement
import Parser.Parsing
import Struct.Value
import Parser.Eval
import Struct.BST

{-| Stores the current state of the system
Tracks command history, variable values and total number of calculations
-}
data State = State { vars :: Tree (Name, Value),
                     funcs :: Tree (Name, Stmt),
                     numCalcs :: Int,
                     history :: [Stmt],
                     results :: [Value]
                   }

-- | Initial system state used for initialising the REPL
initState :: State
initState = State Empty Empty 0 [] []

{-| Updates the variable list with the given name value pair
Searches the variable list for the given name then adds the
name value pair. If the name is already in the list then it is
simply dropped.
-}
updateVars :: Name -> Value -> State -> State
updateVars n v st = st { vars = insert (n, v) (vars st) }

updateFuncs :: Name -> Stmt -> State -> State
updateFuncs n stmt st = st { funcs = insert (n, stmt) (funcs st) }


-- |Add a command to the command history in the state
addHistory :: State -> Stmt -> Value -> State
addHistory state command value = do let st' = updateVars ("it") value state
                                    st' { numCalcs = (numCalcs st')  + 1,
                                    history = (history state) ++ [command],
                                    results = value : (results st') }

fetchHistory :: Int -> State -> Stmt
fetchHistory x st = (history st) !! x

processStmt :: State -> Stmt -> InputT IO State
processStmt st (Stmts x y) = do st' <- processStmt st  x
                                st' <- processStmt st' y
                                return st'

-- | Processing arithmetic and boolean expressions
processStmt st (Eval e) = case val of
                            Right x -> return (addHistory st (Eval e) x)
                            Left x -> do outputStrLn ("Could not evaluate: " ++ x)
                                         return st
                            where val = (eval (vars st) e)

-- | Processing arithmetic and boolean assignments
processStmt st (Set var e) = case (eval (vars st) e) of
  Right x -> do let st' = addHistory (updateVars var x st) (Set var e) x
                return st'
  Left x -> do outputStrLn x
               return st
                                                         

-- |Processing loops and conditionals
processStmt st (If cond stmt) = case (evalB (vars st) cond) of
  Right (B True) -> do st' <- processStmt st stmt 
                       return st'
  Right (B False) -> return st

processStmt st (While cond stmt) = case (evalB (vars st) cond) of
  Right (B True) -> do st' <- processStmt st stmt
                       st'' <- processStmt st' (While cond stmt)
                       return st''
  Right (B False) -> return st


processStmt st (For var cond inc stmt) = case (evalB (vars st) cond) of
  Right (B True) -> do st' <- processStmt st inc
                       st' <- processStmt st' stmt
                       st' <- processStmt st' (For var cond inc stmt)   
                       return st'

  Right (B False) -> return st


processStmt st (Hist e) = case (evalA (vars st) e) of
  Right (I x) -> do st' <- processStmt st (fetchHistory x st)
                    return st'
  Right x -> do (outputStrLn "History commands must evaluate to an integer value")
                return st

  Left  x -> do outputStrLn x
                return st


-- |Processing of functions
processStmt st (Func name stmt) = do let st' =  updateFuncs name stmt st
                                     return st'
                                     
processStmt st (Exec name) = case (valOf name (funcs st)) of
  Right x -> do st' <- processStmt st x
                let st'' = addHistory st (Exec name)  (head (results st'))
                return st''
  Left x -> do outputStrLn (show x)
               return st


processStmt st (Print stmt) = do st' <- processStmt st stmt
                                 outputStrLn (show (head (results st')))
                                 return st'

processStmt st x = do outputStrLn (show x)
                      return st    

processStmts :: State -> Stmt -> InputT IO ()
processStmts st (Stmts x y) = do st' <- processStmt st  x
                                 st' <- processStmt st' y
                                 prompt st'

processStmts st stmt = do st' <- processStmt st stmt
                          prompt st'
 

 
{-| Helper function for the main REPL.
Prints prompt with current calculation count and retrieves the users input
Removes clutter from 'repl'
-}
prompt :: State -> InputT IO ()
prompt st = do inp <- getInputLine (show (numCalcs st) ++ "> ")
               case inp of
                 Just x -> repl st x


{-| Read, Eval, Print Loop
This reads and parses the input using the pCommand parser, and calls
'process' to process the command.
'process' will call 'prompt' when done, so the system loops. 

Before sending the input to be parsed/processed, the input is checked for
user specified operations. These start with ':' and provide different
services to the user.

All currently available operations:

":q" - Quit REPL
":h" - Display help information (to be implemented)
":f FILE_ADDRESS" - Read in and execute commands from a specified file
(to be implemented)

-}
repl :: State -> String -> InputT IO ()
repl st inp 
  | head inp /= ':' =
      case parse pStmts inp of
        [(cmd, "")] -> -- Must parse entire input
          processStmts st cmd
        [(_, x)] -> do outputStrLn ("Parse Error - remaining text: " ++ x)
                       prompt st
        x -> do outputStrLn "Parse Error - Nothing could be parsed"
                prompt st
  | op == 'h' = do printHelp
                   prompt st
  | op == 'l' = do st' <- executeFile st arg
                   outputStrLn ("Loaded file: " ++ arg)
  | op == 'q' = outputStrLn "Bye!"
  | otherwise = do outputStrLn "Not a recognised command"
                   prompt st
  where
    op = head (tail inp)
    arg = (words inp) !! 1
    


executeFile :: State -> String -> InputT IO ()
executeFile st adr = do contents <- liftIO $ readFile adr
                        case parse pStmts contents of
                          [(cmd, "")] -> processStmts st cmd
                        
                          [(_, x)] -> do outputStrLn ("Parse Error - remaining text: " ++ x)
                                         
                          x -> do outputStrLn "Parse Error - Nothing could be parsed"

  
printHelp :: InputT IO ()
printHelp = outputStrLn "No help text available"
