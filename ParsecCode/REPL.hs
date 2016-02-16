module REPL where 

import Text.ParserCombinators.Parsec
import System.Console.Haskeline
import Control.Monad
import Control.Monad.IO.Class
import CalcParser
import BST
import CalcState
import Eval

main :: IO ()
main = runInputT defaultSettings $ prompt initState

prompt :: CalcState -> InputT IO ()
prompt st = do x <- getInputLine (show (numCalcs st) ++ "> ")
               case x of
                 Just input ->
                   case parse statement "" input of
                     Right stmt -> do x <- process st stmt
                                      case x of
                                        Right st' -> prompt st'
                                        Left error -> do outputStrLn error
                                                         prompt st
                                          
                     Left error -> do outputStrLn $ show error
                                      prompt st

printExpr :: CalcState -> Expr -> InputT IO ()
printExpr st expr = outputStrLn $ show $ eval st expr
