module REPL where

import Expr
import Parsing

data State = State { vars :: [(Name, Int)],
                     numCalcs :: Int,
                     history :: [Command] }

initState :: State
initState = State [] 0 []

-- Given a variable name and a value, return a new set of variables with that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name,Int)] -> [(Name,Int)]
updateVars name value set = (name, value) : dropVar name set

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name,Int)] -> [(Name,Int)]
dropVar name set = [(n,v) | (n,v) <- set, name /= n] 

getNthCommand :: State -> Int -> Command
getNthCommand st n = getNthFromList (history st) n

getNthFromList :: [a] -> Int -> a
getNthFromList list pos | pos == 0 = head list
                        | otherwise = getNthFromList (tail list) (pos - 1)

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory state command = state { numCalcs = (numCalcs state)  + 1,
                                   history = (history state) ++ [command] }
toInt :: (Maybe Int) -> Int
toInt (Just x) = x

process :: State -> Command -> IO ()
process st (Set var e) 
  = do let st' = addHistory st {
             vars  = updateVars var (toInt (eval (vars st) e)) (vars st)
             } (Set var e)
       repl st'
       
process st (Fetch e)
  = do let st' = st
       process st (getNthCommand st (toInt (eval (vars st') e)))
           -- st' should include the variable set to the result of evaluating
       
process st (Eval e) 
  = do let st' = addHistory st (Eval e)
       putStr(show (toInt (eval (vars st') e))) -- Print the result of evaluation
       repl st'

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr ("\n" ++ show (numCalcs st) ++ "> ")
             inp <- getLine
             handleInput st inp 

--before any parsing occurs, the input is checked for ':q" 
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
    


  

  -- | (head inp) /= ':' = 
  --     case parse pCommand inp of
  --       [(cmd, "")] -> -- Must parse entire input
  --         process st cmd
  --       _ -> do putStrLn "Parse error"
  --               repl st
  -- | (head (tail inp)) == 'q' = putStrLn "Bye!"
  -- | (head (tail inp)) == 'e' = do (executeFile st (tail (tail inp)))
                                  

  -- | (head (tail inp)) == 'h' = do printHelp
  --                                 repl st

  -- | otherwise = do putStrLn "Not recognised"
  --                  repl st
    


executeFile :: State -> String -> IO ()
executeFile st adr = do contents <- readFile adr
                        putStrLn contents
                        
  
  
printHelp :: IO ()
printHelp = putStrLn "No help text available"
