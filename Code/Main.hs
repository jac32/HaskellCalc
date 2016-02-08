module Main where

import Parser.Parsing
import REPL

main :: IO ()
main = prompt initState
