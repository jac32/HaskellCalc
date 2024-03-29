module Tests.ValueTests where

import Test.HUnit
import Struct.Value

additionValueTests = TestList $ map TestCase [
  (assertEqual "Integer only addition"
   ((Right (I 5)) :: Either String Value)
   (addV (I 3) (I 2))),
  
  (assertEqual "Float only addition"
   ((Right (F 5.5)) :: Either String Value)
   (addV (F 3.2) (F 2.3))),
  
  (assertEqual "Mixed Int Float addition"
   ((Right (F 5.5)) :: Either String Value)
   (addV (I 3) (F 2.5)))
  ]

subtractionValueTests = TestList $ map TestCase [
  (assertEqual "Integer only subtraction"
   ((Right (I 5)) :: Either String Value)
   (subV (I 8) (I 3))),
  
  (assertEqual "Float only subtraction"
   ((Right (F 4.88)) :: Either String Value)
   (subV (F 9.0) (F 4.12))),
  
  (assertEqual "Mixed Int Float subtraction"
   ((Right (F 5.5)) :: Either String Value)
   (subV (I 8) (F 2.5)))
  ]

multiplicationValueTests = TestList $ map TestCase [
  (assertEqual "Integer only multiplication"
   ((Right (I 10)) :: Either String Value)
   (mulV (I 5) (I 2))),
  
  (assertEqual "Float only multiplication"
   ((Right (F 8.25)) :: Either String Value)
   (mulV (F 3.3) (F 2.5))),
  
  (assertEqual "Mixed Int Float multiplication"
   ((Right (F 5.5)) :: Either String Value)
   (mulV (I 5) (F 1.1)))
  ]

divisionValueTests = TestList $ map TestCase [
  (assertEqual "Integer only division"
   ((Right (I 10)) :: Either String Value)
   (divV (I 100) (I 10))),
  
  (assertEqual "Float only division"
   ((Right (F 2.0)) :: Either String Value)
   (divV (F 2.2) (F 1.1))),
  
  (assertEqual "Mixed Int Float division"
   ((Right (F 2.5)) :: Either String Value)
   (divV (I 5) (F 2.0))), 

  (assertEqual "Integer division by zero"
   ((Left "Cannot divide by zero."):: Either String Value)
   (divV (I 5) (I 0))),

  (assertEqual "Float division by zero"
   ((Left "Cannot divide by zero."):: Either String Value)
   (divV (F 25.0) (F 0))),

  (assertEqual "Mixed division by zero a"
   ((Left "Cannot divide by zero."):: Either String Value)
   (divV (I 2) (F 0))),

  (assertEqual "Mixed division by zero b"
   ((Left "Cannot divide by zero."):: Either String Value)
   (divV (F 4.0) (I 0)))
  ]


notValueTests = TestList $ map TestCase [
  (assertEqual "NOT operation on True"
   (Right (B False) :: Either String Value)
   (notV (B True))),

  (assertEqual "NOT operation on False"
   (Right (B True) :: Either String Value)
   (notV (B False))),

  (assertEqual "NOT operation on Integer"
   ((Left "Logical NOT on incompatible types") :: Either String Value)
   (notV (I 3)))
  ]

andValueTests = TestList $ map TestCase [
  (assertEqual "AND operation True && True"
   (Right (B True) :: Either String Value)
   (andV (B True) (B True))),

  (assertEqual "AND operation True && False"
   (Right (B False) :: Either String Value)
   (andV (B True) (B False))),

   (assertEqual "AND operation False && True"
   (Right (B False) :: Either String Value)
   (andV (B False) (B True))),
  
  (assertEqual "AND operation False && False"
   (Right (B False) :: Either String Value)
   (andV (B False) (B False))),

  (assertEqual "AND operation on Integers"
   ((Left "Logical AND on incompatible types") :: Either String Value)
   (andV (I 2) (I 3))),

  (assertEqual "AND operation on Floats"
   ((Left "Logical AND on incompatible types") :: Either String Value)
   (andV (F 2.2) (F 3.3))),

  (assertEqual "AND operation on mixed number types A"
   ((Left "Logical AND on incompatible types") :: Either String Value)
   (andV (F 2.2) (I 3))),
  
  (assertEqual "AND operation on mixed number types B"
   ((Left "Logical AND on incompatible types") :: Either String Value)
   (andV (I 2) (F 3.5)))
  ]


orValueTests = TestList $ map TestCase [
   (assertEqual "OR operation True && True"
   (Right (B True) :: Either String Value)
   (orV (B True) (B True))),

  (assertEqual "OR operation True && False"
   (Right (B True) :: Either String Value)
   (orV (B True) (B False))),

   (assertEqual "OR operation False && True"
   (Right (B True) :: Either String Value)
   (orV (B False) (B True))),

  (assertEqual "OR operation False && False"
   (Right (B False) :: Either String Value)
   (orV (B False) (B False))),

  (assertEqual "OR operation on Integers"
   ((Left "Logical OR on incompatible types") :: Either String Value)
   (orV (I 2) (I 3))),

  (assertEqual "OR operation on Floats"
   ((Left "Logical OR on incompatible types") :: Either String Value)
   (orV (F 2.2) (F 3.3))),

  (assertEqual "OR operation on mixed number types A"
   ((Left "Logical OR on incompatible types") :: Either String Value)
   (orV (F 2.2) (I 3))),
  
  (assertEqual "OR operation on mixed number types B"
   ((Left "Logical OR on incompatible types") :: Either String Value)
   (orV (I 2) (F 3.5)))
  ]


eqValueTests = TestList $ map TestCase [
   (assertEqual "EQ operation on ints 2==2"
   (Right (B True) :: Either String Value)
   (eqV ((I 2)) ((I 2)))),
   
   (assertEqual "EQ operation on float and int 2.0==2"
   (Right (B True) :: Either String Value)
   (eqV ((F 2.0)) ((I 2)))),
   
   (assertEqual "EQ operation on float and int (false) 2.2==2"
   (Right (B False) :: Either String Value)
   (eqV ((F 2.2)) ((I 2)))),

   (assertEqual "EQ operation on Floats2.2==2.2"
   (Right (B True) :: Either String Value)
   (eqV ((F 2.2)) ((F 2.2)))),

   (assertEqual "EQ operation on Bools True==True"
   (Left "Equality check of incompatible types." :: Either String Value)
   (eqV ((B True)) ((B True)))),

   (assertEqual "EQ operation on incompatible types 5==True"
   (Left "Equality check of incompatible types." :: Either String Value)
   (eqV ((I 5)) ((B True))))
  ]


ltValueTests = TestList $ map TestCase [
   (assertEqual "LT operation on floats 2<3"
   (Right (B True) :: Either String Value)
   (ltV ((I 2)) ((I 3)))),

   (assertEqual "LT operation 3<2"
   (Right (B False) :: Either String Value)
   (ltV ((I 3)) ((I 2)))),

   (assertEqual "LT operation on floats 2.0<3.0"
   (Right (B True) :: Either String Value)
   (ltV ((F 2.0)) ((F 3.0)))),
   
   (assertEqual "LT operation on float and int 2.0 < 3"
   (Right (B True) :: Either String Value)
   (ltV ((F 2.0)) ((I 3)))),
   
   (assertEqual "Lt operation on incompatible types 5<True"
   (Left "Order check of incompatible types." :: Either String Value)
   (ltV ((I 5)) ((B True))))
  ]

gtValueTests = TestList $ map TestCase [
   (assertEqual "GT operation on floats 3>2"
   (Right (B True) :: Either String Value)
   (gtV ((I 3)) ((I 2)))),

   (assertEqual "GT operation 2>3"
   (Right (B False) :: Either String Value)
   (gtV ((I 2)) ((I 3)))),

   (assertEqual "GT operation on floats 3.0>2.0"
   (Right (B True) :: Either String Value)
   (gtV ((F 3.0)) ((F 2.0)))),
   
   (assertEqual "GT operation on float and int 3.0 > 2"
   (Right (B True) :: Either String Value)
   (gtV ((F 3.0)) ((I 2)))),
   
   (assertEqual "Gt operation on incompatible types 5>True"
   (Left "Order check of incompatible types." :: Either String Value)
   (gtV ((I 5)) ((B True))))
   ] 

sqrtValueTests = TestList $ map TestCase [
   (assertEqual "sqrt operation on float 2.2"
   (Right (F 1.4832398) :: Either String Value)
   (sqrtV (F 2.2) )),

   (assertEqual "sqrt operation on int 9"
   (Right (F 3.0) :: Either String Value)
   (sqrtV (I 9) )),
  
   (assertEqual "Sqrt operation on incompatible type True"
   (Left "Square root with incompatible types." :: Either String Value)
   (sqrtV (B True)))
   ] 

factValueTests = TestList $ map TestCase [
   (assertEqual "fact operation on float 2.2"
   (Left "Factorial with incompatible types." :: Either String Value)
   (factV (F 2.2) )),

   (assertEqual "fact operation on int 5"
   (Right (I 120) :: Either String Value)
   (factV (I 5) )),
  
   (assertEqual "Fact operation on incompatible type True"
   (Left "Factorial with incompatible types." :: Either String Value)
   (factV (B True)))
   ]
                     
modValueTests = TestList $ map TestCase [
   (assertEqual "Mod operation on int and float"
   (Left "Modulo with incompatible types." :: Either String Value)
   (modV (I 2) (F 2.2) )),

   (assertEqual "mod operation on int 5 and 2"
   (Right (I 1) :: Either String Value)
   (modV (I 5) (I 2 ))),
  
   (assertEqual "Mod operation on incompatible type Float and True"
   (Left "Modulo with incompatible types." :: Either String Value)
   (modV (F 7.2) (B True)))
   ]
 
absValueTests = TestList $ map TestCase [
   (assertEqual "Abs operation on positive int"
   (Right (I 2) :: Either String Value)
   (absV (I 2))),
  
   (assertEqual "Abs operation on negative int"
   (Right (I 2) :: Either String Value)
   (absV (I (-2)))),
  
   (assertEqual "Abs operation on positive float"
   (Right (F 2.43) :: Either String Value)
   (absV (F 2.43))),
  
   (assertEqual "Abs operation on negative float"
   (Right (F 2.43) :: Either String Value)
   (absV (F (-2.43)))),
   
   (assertEqual "Abs operation on True"
   (Left "Absolute Value of incompatible type" :: Either String Value)
   (absV (B True)))
  ] 


powValueTests = TestList $ map TestCase [
   (assertEqual "Pow operation on int and float"
   (Left "Exponent Operation with incompatible types." :: Either String Value)
   (powV (I 2) (F 2.2) )),

   (assertEqual "Pow operation on float 0.5 and int 2"
   (Right (F 0.25) :: Either String Value)
   (powV (F 0.5) (I 2 ))),
 
   (assertEqual "Pow operation on int 5 and int 2"
   (Right (I 25) :: Either String Value)
   (powV (I 5) (I 2 ))),
  
   (assertEqual "Pow operation on incompatible type Float and True"
   (Left "Exponent Operation with incompatible types." :: Either String Value)
   (powV (F 7.2) (B True)))
   ]                  
