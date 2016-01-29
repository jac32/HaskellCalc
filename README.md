<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Overview</a>
<ul>
<li><a href="#sec-1-1">1.1. Task</a></li>
<li><a href="#sec-1-2">1.2. Starter code</a></li>
<li><a href="#sec-1-3">1.3. Running the Calculator</a></li>
</ul>
</li>
<li><a href="#sec-2">2. Requirements</a>
<ul>
<li><a href="#sec-2-1">2.1. Basic Requirements</a>
<ul>
<li><a href="#sec-2-1-1">2.1.1. Example Session</a></li>
</ul>
</li>
<li><a href="#sec-2-2">2.2. Additional Requirements</a>
<ul>
<li><a href="#sec-2-2-1">2.2.1. Easy</a></li>
<li><a href="#sec-2-2-2">2.2.2. Medium</a></li>
<li><a href="#sec-2-2-3">2.2.3. Hard</a></li>
<li><a href="#sec-2-2-4">2.2.4. Hard</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#sec-3">3. Deliverables</a>
<ul>
<li><a href="#sec-3-1">3.1. Code</a></li>
<li><a href="#sec-3-2">3.2. Report</a></li>
</ul>
</li>
</ul>
</div>
</div>


# Overview<a id="sec-1" name="sec-1"></a>

## Task<a id="sec-1-1" name="sec-1-1"></a>

Create an interactive calculator application, supporting:

-   variables
-   expression evaluation
-   error handling
-   command history

## Starter code<a id="sec-1-2" name="sec-1-2"></a>

Some code was given as a starting point.

-   `Main.hs`, which initialises the REPL

    -- Main.hs
    module Main where
    
    import Parsing
    import Expr
    import REPL
    
    main :: IO ()
    main = repl initState

-   `REPL.hs`, containing the REPL and **incomplete** helper functions

    -- REPL.hs
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
    updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
    updateVars = undefined
    
    -- Return a new set of variables with the given name removed
    dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
    dropVar = undefined
    
    -- Add a command to the command history in the state
    addHistory :: State -> Command -> State
    addHistory = undefined
    
    process :: State -> Command -> IO ()
    process st (Set var e) 
         = do let st' = undefined
              -- st' should include the variable set to the result of evaluating e
              repl st'
    process st (Eval e) 
         = do let st' = undefined
              -- Print the result of evaluation
              repl st'
    
    -- Read, Eval, Print Loop
    -- This reads and parses the input using the pCommand parser, and calls
    -- 'process' to process the command.
    -- 'process' will call 'repl' when done, so the system loops.
    
    repl :: State -> IO ()
    repl st = do putStr (show (numCalcs st) ++ " > ")
                 inp <- getLine
                 case parse pCommand inp of
                      [(cmd, "")] -> -- Must parse entire input
                              process st cmd
                      _ -> do putStrLn "Parse error"
                              repl st

-   `Expr.hs`, containing the definition of a *basic* expression type
    and an **incomplete** evaluator for expressions

    module Expr where
    
    import Parsing
    
    type Name = String
    
    -- At first, 'Expr' contains only addition and values. You will need to 
    -- add other operations, and variables
    data Expr = Add Expr Expr
              | Val Int
      deriving Show
    
    -- These are the REPL commands - set a variable name to a value, and evaluate
    -- an expression
    data Command = Set Name Expr
                 | Eval Expr
      deriving Show
    
    eval :: [(Name, Int)] -> -- Variable name to value mapping
            Expr -> -- Expression to evaluate
            Maybe Int -- Result (if no errors such as missing variables)
    eval vars (Val x) = Just x -- for values, just give the value directly
    eval vars (Add x y) = Nothing -- return an error (because it's not implemented yet!)
    
    digitToInt :: Char -> Int
    digitToInt x = fromEnum x - fromEnum '0'
    
    pCommand :: Parser Command
    pCommand = do t <- letter
                  char '='
                  e <- pExpr
                  return (Set [t] e)
                ||| do e <- pExpr
                       return (Eval e)
    
    pExpr :: Parser Expr
    pExpr = do t <- pTerm
               do char '+'
                  e <- pExpr
                  return (Add t e)
                ||| do char '-'
                       e <- pExpr
                       error "Subtraction not yet implemented!" 
                     ||| return t
    
    pFactor :: Parser Expr
    pFactor = do d <- digit
                 return (Val (digitToInt d))
               ||| do v <- letter
                      error "Variables not yet implemented" 
                    ||| do char '('
                           e <- pExpr
                           char ')'
                           return e
    
    pTerm :: Parser Expr
    pTerm = do f <- pFactor
               do char '*'
                  t <- pTerm
                  error "Multiplication not yet implemented" 
                ||| do char '/'
                       t <- pTerm
                       error "Division not yet implemented" 
                     ||| return f

-   `Parsing.hs`, which contains the parsing library discussed in lectures

    THE CODE FOR THIS BLOCK IS CURRENTLY UNAVAILABLE ON STUDRES

We should only need to change `Expr.hs` and `REPL.hs` and should 
report any other changes 

## Running the Calculator<a id="sec-1-3" name="sec-1-3"></a>

To run the calculator, we can either: 

-   Load `Main.hs` into ghci and run the function `main`.
-   Compile to an executable with:

    ghc --make Main.hs -o calc
    ./calc

# Requirements<a id="sec-2" name="sec-2"></a>

## Basic Requirements<a id="sec-2-1" name="sec-2-1"></a>

The minimum requirement of this project is to implement the 
sections of the provided code which are currently undefined
or applications of error.

These are found in the two given files which we are to complete:
-   `REPL.hs`
    -   `updateVars` - a function for storing and updating variables
    -   `dropVar` - a function for removing a variable from store
    -   `addHistory` - a function to add a command to the history
    -   `process` - the function which performs the command in the current state

-   `Expr.hs`
    -   `eval` - complete the evaluation function for addition
    -   `pExpr` - define behaviour upon subtraction expression parsing
    -   `pFactor` - define behaviour for variable parsing
    -   `pTerm` - define behaviour for multiplication and

In addition we should include:

-   A **quit** command, which exits the calculator
-   Support for variable assignment using the `Set` command
-   The evaluator `State`, for keeping track of:
    -   The current values of variables
    -   The number of calculations performed (show in prompt)
    -   The command history
-   A command for accesing command history
-   Parser support for 
    -   multiple digit numbers
    -   whitespace
    -   **optional** other features

### Example Session<a id="sec-2-1-1" name="sec-2-1-1"></a>

An example session with the basic working version might look like the following:

    0 > 4+5
    9
    1 > x=9
    OK
    1 > x=x+1 OK
    1>x
    10
    2 > !1 OK 2>x 11
    3 > 4+8
    12
    4 > it+4
    16
    4 > :q Bye

## Additional Requirements<a id="sec-2-2" name="sec-2-2"></a>

Once the basic requirements are complete then their are a list of
possible extensions which we can attempt. 
The basic alone can only achieve up to a 13 and, for a 17 or above,
all of the suggested **Medium** requirements should be completed

### Easy<a id="sec-2-2-1" name="sec-2-2-1"></a>

-   Extend the parser to support negative integers
-   Support additional functionsm such as `abs`, `mod` or `power`
-   Support floating point numbers **instead** of integers

### Medium<a id="sec-2-2-2" name="sec-2-2-2"></a>

-   Support floating point numbers **and** integers
-   Add a command to read input files containing a list of commands
-   Implement better error handling (look at the `Either` type)

### Hard<a id="sec-2-2-3" name="sec-2-2-3"></a>

-   Add commands for printing and looping

### Hard<a id="sec-2-2-4" name="sec-2-2-4"></a>

-   Look at the Haskell libraries and consider how to improve input
    in particular look at [Haskeline](http://hackage.haskell.org/package/haskeline)
-   Allow defining functions as well as variables (**very** hard)

# Deliverables<a id="sec-3" name="sec-3"></a>

-   Deadline is 9pm Tuesday of Week 4
-   A single zip should be submitted containing subdirectories
    -   `Code`
    -   `Report`

## Code<a id="sec-3-1" name="sec-3-1"></a>

The `Code` directory should contian the group's code and should be the
same for all team members.
Everything needed to run the application should be in that directory
or be part of the Haskell standard library

## Report<a id="sec-3-2" name="sec-3-2"></a>

The `Report` directory should contain an *individual* PDF report 
(1500-2000 words).
In particular, it should include:

-   A summary of the functionality of the program indicating the 
    level of completeness with respect of the requirements
-   Any known problems with your code
-   Any specific problems you encountered which you were able to solve
    and how you solved them
-   An accurate summary of provenance, i.e. stating which files or 
    code fragments were
    1.  Written by you
    2.  Modified by you from the source files provided for this 
        assignment
    3.  Sourced from elsewhere and who wrote them
-   A description of **your own contribution** to the group work
