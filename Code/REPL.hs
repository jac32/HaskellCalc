module REPL where

import Expr
import Parsing


{-| Stores the current state of the system
Tracks command history, variable values and total number of calculations
-}
data State = State { vars :: [(Name, Int)],
                     numCalcs :: Int,
                     history :: [Command] }

-- | Initial system state used for initialising the REPL
initState :: State
initState = State [] 0 []

{-| Updates the variable list with the given name value pair
Searches the variable list for the given name then adds the
name value pair. If the name is already in the list then it is
simply dropped.
-}
updateVars :: Name -> Int -> [(Name,Int)] -> [(Name,Int)]
updateVars name value vars = (name, value) : dropVar name vars


-- | Removes the given name and matching value from the list of variables
dropVar :: Name -> [(Name,Int)] -> [(Name,Int)]
dropVar name vars = [(n,v) | (n,v) <- vars, name /= n] 


{-| REDUNDANT Fetches the nth command from the state's command history
The indexing matches the prompt number printed in the repl when the
command was originally entered. i.e., The first command entered is
n = 0. This is just the most common use of 'getNthFromList'
-}
getNthCommand :: State -> Int -> Command
getNthCommand st n = (history st) !! n

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory state command = state { numCalcs = (numCalcs state)  + 1,
                                   history = (history state) ++ [command] }

-- | HACK: Converts maybe Ints to ints 
toInt :: (Maybe Int) -> Int
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
       repl st'
       
process st (Fetch e)
  = do let st' = st
       process st ((history st) !! (toInt (eval (vars st') e)))
           -- st' should include the variable set to the result of evaluating
       
process st (Eval e) 
  = do let st' = addHistory st (Eval e)
       putStr(show (toInt (eval (vars st') e))) -- Print the result of evaluation
       repl st'
{-|
Read, Eval, Print Loop
This reads and parses the input using the pCommand parser, and calls
'process' to process the command.
'process' will call 'repl' when done, so the system loops.
-}
repl :: State -> IO ()
repl st = do putStr ("\n" ++ show (numCalcs st) ++ "> ")
             inp <- getLine
             handleInput st inp 



handleInput :: State -> String -> IO ()
handleInput st inp 
  | head inp /= ':' =
      case parse pCommand inp of
        [(cmd, "")] -> -- Must parse entire input
          process st cmd
        _ -> do putStrLn "Parse error"
                repl st
  | op == 'h' = do printHelp
                   repl st
  | op == 'e' = do executeFile st arg
                   repl st
  | op == 'q' = putStrLn "Bye!"
  | otherwise = do putStrLn "Not a recognised command"
                   repl st
  where
    op = head (tail inp)
    arg = (words inp) !! 1
    

executeFile :: State -> String -> IO ()
executeFile st adr = do contents <- readFile adr
                        putStrLn contents
  
printHelp :: IO ()
printHelp = putStrLn "No help text available"
