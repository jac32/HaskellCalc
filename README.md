

Haskell Calc
================

Version 1.0 (with Parsec)

What
-----

Haskell Calc is a calculator written in haskell which can be interacted with via the command line.
It supports:
	- Standard Arithmetic Operations: addition, subtraction, multiplication and division.
	- Boolean Operations: Not, And and Or
	- Relational Operations: Less than, Greater than and Equal to 
	- Simple scripting including : conditionals, looping and functions
	
How 
----

The interface supports a number of statement types:

### Prompt Interaction
- `quit` will exit the calculator prompt
- `help` will display this README file 
- `load adr` will load the file found at the relative address `adr` and evaluate its contents
- `! n` will reevaluate the nth statement in history (The statement number is displayed in the prompt)

### Script Statements
- `print(e)` will evaluate and display the arithmetic or boolean expression
- `if (e) then {stmt1} else {stmt2}` will evaluate e and then run the corresponding stmt 
  (braces and brackets are optional)
- `while (e) then {stmt}` will repeatedly run the stmt until the expression evaluates to false
  (braces and brackets are optional)
- `defun NAME (COMMA,SEPERATED,ARGUMENT,NAMES) {stmt}` will store the stmt as an executable function
  (braces are optional, brackets are NOT)
- `NAME(COMMA,SEPARATED,EXPRS)` will execute the function with the given name, using the results from
  evaluating the expressions as arguments
  (if more arguments are supplied than originally declared in function definition, the additional variables are ignored)
  
### Expression Syntax
Expressions are whitespace insensitive (for arithmetic), have the expected precedence rules and are left associative.
There are two prefix operators: 
- Boolean Not which is written "not" 
- The arithmetic negativity operator "-"
To apply these operators multiple times brackets must be used i.e, `-(-2) = 2` but `--2` is undefined


All other operators are binary. These are listed below:
- Arithmetic: `+`, `-`, `*`, `/`
- Boolean: `and`, `or`
- Relational: `<`, `=`, `>`

