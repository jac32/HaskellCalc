module CalcState where

import BST
import CalcParser

{-| Stores the current state of the system
Tracks command history, variable values, functions recorded and total number of calculations
-}
data CalcState = CalcState { vars :: Tree (Name, Value),
                             funcs :: Tree (Name, Function),
                             numCalcs :: Int,
                             history :: [Stmt]
                           }

data Function = Function { args :: [Name],
                           body :: Stmt }

-- | Initial system state used for initialising the REPL
initState :: CalcState
initState = CalcState Empty Empty 0 [] 

-- | This function updates the BST of variables in the provided CalcState, by adding
-- the new variable with the provided name, and value to the BST
updateVars :: Name -> Value -> CalcState -> CalcState
updateVars n v st = st { vars = insert (n, v) (vars st) }


-- | This function updates the BST of functions in the provided CalcState, by adding
-- the new function with the provided name, args, and stmt to the BST
updateFuncs :: Name -> [Name] -> 
               Stmt -> CalcState -> CalcState 
updateFuncs n args stmt st = st { funcs = insert (n, (Function args stmt)) (funcs st) }


-- | This function retrieves the nth element of the provided list, and returns
-- either an error message (if the index is invalid) or the corresponding element
getN :: [a] -> Integer -> Either String a
getN (x:xs) 0 = Right x
getN (x:xs) n = getN xs $ n-1
getN [] n = Left "Index out of bounds"

-- | This function is used to generate a new state unique to the function being
-- called. It is used to create a local scope for the function, so it has its own
-- BST of functions and variables.
genFuncState :: CalcState -> Function -> [Value] -> CalcState
genFuncState st function values = initState { funcs = (funcs st), 
                                              vars  = (insertList pairs Empty) }
    where names  = args function
          pairs  = zip names values
                                                 
                                     
-- Add a command to the command history in the state
addHistory :: CalcState -> Stmt -> CalcState
addHistory state command =  state { numCalcs = (numCalcs state)  + 1,
                                history = (history state) ++ [command] }
                                  
-- | This function is called to retrieve a previously exectuted statement
-- by calling the helper function getN
fetchHistory :: CalcState -> Integer -> Either String Stmt
fetchHistory st x = getN (history st) x
