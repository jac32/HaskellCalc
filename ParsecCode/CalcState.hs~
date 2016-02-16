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

updateVars :: Name -> Value -> CalcState -> CalcState
updateVars n v st = st { vars = insert (n, v) (vars st) }

updateFuncs :: Name -> [Name] ->
               Stmt -> CalcState -> CalcState
updateFuncs n args stmt st = st { funcs = insert (n, (Function args stmt)) (funcs st) }

genFuncState :: CalcState -> Function -> [Value] -> CalcState
genFuncState st function values = initState { funcs = (funcs st), 
                                              vars  = (insertList pairs Empty) }
    where names  = args function
          pairs  = zip names values
    


 
                                                 
                                     
-- Add a command to the command history in the state
addHistory :: CalcState -> Stmt -> Value -> CalcState
addHistory state command value =
  updateVars "it" value state { numCalcs = (numCalcs state)  + 1,
                                history = (history state) ++ [command] }
                                                                     

fetchHistory :: Int -> CalcState -> Stmt
fetchHistory x st = (history st) !! x
