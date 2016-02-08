module Parser.Eval where

import Struct.BST
import Struct.Statement
import Struct.Value
             
evalA :: Tree (Name, Value)->     
        AExpr ->               
        Either String Value

evalB :: Tree (Name, Value)->     
         BExpr ->               
         Either String Value
  
-----------------------------------------------------------------
-- Boolean Expression Evaluations
-----------------------------------------------------------------
evalB vars (Const b) = Right (B b)

evalB vars (BVar v)  = (valOf v vars)

evalB vars (Not b)   = bUnFrame vars b notV

evalB vars (Or a b)  = bBinFrame vars a b orV

evalB vars (And a b) = bBinFrame vars a b andV

evalB vars (Eq x y)  = binFrame vars x y eqV

evalB vars (Gt x y)  = binFrame vars x y gtV

evalB vars (Lt x y)  = binFrame vars x y ltV

-----------------------------------------------------------------
-- Arithmetic Expression Evaluations
-----------------------------------------------------------------

evalA vars (Val x)    = Right x

evalA vars (AVar v)   = (valOf v vars)

evalA vars (Neg e)    = unFrame vars e negV
  
evalA vars (Pow x y)  = binFrame vars x y powV
  
evalA vars (Add x y)  = binFrame vars x y addV

evalA vars (Sub x y)  = binFrame vars x y subV

evalA vars (Mul x y)  = binFrame vars x y mulV

evalA vars (Div x y)  = binFrame vars x y divV

evalA vars (Mod x y)  = binFrame vars x y modV

evalA vars (Pow x y)  = binFrame vars x y powV



-----------------------------------------------------------------
-- Function Application Frames
-----------------------------------------------------------------


binFrame :: Tree (Name, Value) ->
             AExpr -> 
             AExpr ->
             (Value -> Value -> Either String Value) ->
             Either String Value

binFrame vars x y z = case (evalA vars x, evalA vars y) of
  (Right x', Right y') -> z x' y'
  (Left x', _) -> Left x'
  (_, Left y') -> Left y'


unFrame :: Tree (Name, Value) ->
           AExpr -> 
           (Value -> Either String Value) ->
           Either String Value
             
unFrame vars x y = case (evalA vars x) of
  (Right x') -> y x'
  (Left x') -> Left x'


bBinFrame :: Tree (Name, Value) ->
             BExpr -> 
             BExpr ->
             (Value -> Value -> Either String Value) ->
             Either String Value

bBinFrame vars x y z = case (evalB vars x, evalB vars y) of
  (Right x', Right y') -> z x' y'
  (Left x', _) -> Left x'
  (_, Left y') -> Left y'

bUnFrame :: Tree (Name, Value) ->
           BExpr -> 
           (Value -> Either String Value) ->
           Either String Value
             
bUnFrame vars x y = case (evalB vars x) of
  (Right x') -> y x'
  (Left x') -> Left x'
