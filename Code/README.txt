In order to compile and run the program, you can simply run the executable by typing 

        ./calc 

into the terminal.
This was compiled using the command:
        
        ghc --make Main.hs -o calc

--------------------------------------------------------

The program is quite straight forward in how it operates. Arithmetic expressions can be entered in
as one would expect. There is support for factorials, absolute values, exponents, floats and ints, modulo, square root
as well as the standard subtraction, addition, multiplication and division.

example:

                (2+2)*3^|-4%1|

This evaluates to 4, as one would expect given order of operations
----------------------------------------------------------

Loops and functions are supported as well, and can be entered as one would expect;
the coniditons for if statements, for and while loops are entered within brackets, with
the bodies of the functions enclosed in curly braces. 
Multiple statements can be separated by semi-colons, with the last statement not needing one

example:

        func loop {x=0; while (x<100) {x=x+1}}
        loop()

------------------------------------------------------------
NOTE: Functions have separate scope. The following  will create an infinite loop, as the 'x' in inc() is
different than the one in loop(). As a result, the 'x' in loop never actually gets incremented. 

        func inc {x=x+1}
        func loop {x=0; while (x<100) {inc()}}
        loop()
-------------------------------------------------------------
NOTE: UNLESS 'print' is specifically written, the stmt will be evaluated without anything being printed
to the console. INCLUDE 'print' at the start if you want to see the result.
This also applies to functions and loops, with print merely printing the final value in which they calculate.

example:

        x=0
        print if(x<10){x=10}

will print '10.' It is identical to:

        x=0
        print if(x<10){x=10;print x}

-------------------------------------------------------
For loading files, the command is:

        :l fileName.txt

In the file, please note that it is read in as if everything was on a single
line, which mandates a semi-colon on the end of each statement, regardless of new line
characters. This, of course, does not apply to the final line

------------------------------------------------------
A test suite was implemented and can be called using the following command
from within the 'Code' directory:

        ghci Tests/TestController.hs
        runTestTT allTests

