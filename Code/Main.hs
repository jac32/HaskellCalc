module Main where

import System.Console.Haskeline
import Parser.Parsing
import REPL

main :: IO ()
main = runInputT defaultSettings $ prompt initState
