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
main = do putStrLn printWelcome
          runInputT defaultSettings $ prompt initState

prompt :: CalcState -> InputT IO ()
prompt st = do x <- getInputLine (show (numCalcs st) ++ "> ")
               case x of
                 Just input ->
                    do y <- parseStatement st input
                       case y of 
                         Right x -> prompt x
                         Left "exit" -> outputStrLn "Bye!"
                         Left error -> do outputStrLn error
                                          prompt st

                   

printExpr :: CalcState -> Expr -> InputT IO ()
printExpr st expr = outputStrLn $ show $ eval st expr

printWelcome :: String
printWelcome = "============================================================"
               ++ "\nWelcome to Haskell Calc!\n" ++
               "============================================================"
               ++ "\nEnter \"help\" for more information.\n"
