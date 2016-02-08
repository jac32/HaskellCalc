module REPL where

import Struct.Statement
import Parser.Parsing
import Struct.Value
import Parser.Eval
import Struct.BST

{-| Stores the current state of the system
Tracks command history, variable values and total number of calculations
-}
data State = State { vars :: Tree (Name, Value),
                     numCalcs :: Int,
                     history :: [Stmt] }

-- | Initial system state used for initialising the REPL
initState :: State
initState = State Empty 0 []

{-| Updates the variable list with the given name value pair
Searches the variable list for the given name then adds the
name value pair. If the name is already in the list then it is
simply dropped.
-}
updateVars :: Name -> Value -> Tree (Name,Value) -> Tree (Name,Value)
updateVars name value vars = insert (name, value) vars


-- | Removes the given name and matching value from the list of variables
dropVar :: Name -> Tree (Name,Value) -> Tree (Name,Value)
dropVar name vars = remove name vars

-- Add a command to the command history in the state
addHistory :: State -> Stmt -> State
addHistory state command = state { numCalcs = (numCalcs state)  + 1,
                                   history = (history state) ++ [command] }

-- | HACK: Converts values to ints
valToInt :: Value -> Int
valToInt (I x) =  x
valToInt _ = undefined

{-| Performs the correct action for the entered commmand in a given state
3 main commands to be processed:
1. Setting variable - Update variable list and continue REPL with new state
2. Fetching from history - Evaluate expression to find out which command to fetch and then execute
3. Evaluate expression - Add the evaluation to the command history, evaluate the expression and print result before continuing with the REPL
-}
process :: State -> Stmt -> IO ()
process st (ASet var e) = case (evalA (vars st) e) of
  Right x -> do let st' = addHistory st {
                      vars  = updateVars var x (vars st)
                      } (ASet var e)
                prompt st'
  Left x -> do putStrLn x
               prompt st


process st (BSet var e) = case (evalB (vars st) e) of
  Right x -> do let st' = addHistory st {
                      vars = updateVars var x (vars st)
                      } (BSet var e)
                prompt st'
  Left x -> do putStrLn x
               prompt st
       
           -- st' should include the variable set to the result of evaluating
process st (BEval e)
  = do let st' = addHistory st (BEval e)
       putStr(show (evalB (vars st') e)) -- Print the result of evaluation
       prompt st'

process st (AEval e)
  = do let st' = addHistory st (AEval e)
       putStr(show (evalA (vars st') e)) -- Print the result of evaluation
       prompt st'

-- process st x
--   = do putStrLn (show x)
--        prompt st
       
{-| Helper function for the main REPL.
Prints prompt with current calculation count and retrieves the users input
Removes clutter from 'repl'
-}
prompt :: State -> IO ()
prompt st = do putStr ("\n" ++ show (numCalcs st) ++ "> ")
               inp <- getLine
               repl st inp 


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
repl :: State -> String -> IO ()
repl st inp 
  | head inp /= ':' =
      case parse pStmt inp of
        [(cmd, "")] -> -- Must parse entire input
          process st cmd
        [(_, x)] -> do putStrLn ("Parse Error - remaining text: " ++ x)
                       prompt st
        x -> do putStrLn "Parse Error - Nothing could be parsed"
                prompt st
  | op == 'h' = do printHelp
                   prompt st
  | op == 'f' = do executeFile st arg
                   prompt st
  | op == 'q' = putStrLn "Bye!"
  | otherwise = do putStrLn "Not a recognised command"
                   prompt st
  where
    op = head (tail inp)
    arg = (words inp) !! 1
    


executeFile :: State -> String -> IO ()
executeFile st adr = do contents <- readFile adr
                        putStrLn contents
  
printHelp :: IO ()
printHelp = putStrLn "No help text available"
