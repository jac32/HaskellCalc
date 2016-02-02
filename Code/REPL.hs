module REPL where

import Expr
import Parsing

data State = State { vars :: [(Name, Int)],
                     numCalcs :: Int,
                     history :: [Command] }

initState :: State
initState = State [] 0 []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name ->Int -> [(Name,Int)] -> [(Name,Int)]
updateVars name value set = (name, value) : dropVar name set

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name,Int)] -> [(Name,Int)]
dropVar name set = [(n,v) | (n,v) <- set, name /= n] 

-- Return the value of the named integer
-- getVar :: Name -> [(Name, Int)] -> Int
-- getVar v = (snd (head [(name, val) | (name, val) <- vars, name == v]))

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory state command = state { numCalcs = (numCalcs state)  + 1,
                                   history = command : (history state) }
toInt :: (Maybe Int) -> Int
toInt (Just x) = x

process :: State -> Command -> IO ()
process st (Set var e) 
  = do let st' = addHistory st { vars  = updateVars var (toInt (eval (vars st) e)) (vars st) } (Set var e)

                
           -- st' should include the variable set to the result of evaluating
       repl st'
process st (Eval e) 
  = do let st' = addHistory st (Eval e)
       putStrLn(show (eval (vars st') e)) -- Print the result of evaluation
       repl st'


-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- getLine
             handleInput st inp 

--before any parsing occurs, the input is checked for ':q" 
handleInput :: State -> String -> IO ()
handleInput st inp 
  | (inp /= ":q") = 
      case parse pCommand inp of
        [(cmd, "")] -> -- Must parse entire input
          process st cmd
        _ -> do putStrLn "Parse error"
                repl st
  | otherwise = putStrLn "Bye"
