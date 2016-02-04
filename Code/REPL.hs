module REPL where

import Expr
import Parsing
import Value
import BST

{-| Stores the current state of the system
Tracks command history, variable values and total number of calculations
-}
data State = State { vars :: Tree (Name, Value),
                     numCalcs :: Int,
                     history :: [Command] }

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
addHistory :: State -> Command -> State
addHistory state command = state { numCalcs = (numCalcs state)  + 1,
                                   history = (history state) ++ [command] }

-- | HACK: Converts maybe Ints to ints 
toInt :: (Maybe Value) -> Value
toInt (Just x) = x
toInt Nothing = undefined

{-| Performs the correct action for the entered commmand in a given state
3 main commands to be processed:
1. Setting variable - Update variable list and continue REPL with new state
2. Fetching from history - Evaluate expression to find out which command to fetch and then execute
3. Evaluate expression - Add the evaluation to the command history, evaluate the expression and print result before continuing with the REPL
-}
process :: State -> Command -> IO ()
process st (Set var e) 
  = do let st' = addHistory st {
             vars  = updateVars var (toInt (eval (vars st) e)) (vars st)
             } (Set var e)
       prompt st'
       
process st (Fetch e)
  = do let st' = st
       process st ((history st) !! 2)--(toInt (eval (vars st') e)))
           -- st' should include the variable set to the result of evaluating
       
process st (Eval e) 
  = do let st' = addHistory st (Eval e)
       putStr(drop 2 (show (toValue (eval (vars st') e)))) -- Print the result of evaluation
       prompt st'

       
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
      case parse pCommand inp of
        [(cmd, "")] -> -- Must parse entire input
          process st cmd
        _ -> do putStrLn "Parse error"
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
