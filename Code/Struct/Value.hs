module Struct.Value where

data Value = I Int
  | F Float
  | B Bool
  | S String
  deriving (Eq, Show)

--Addition, Subtraction, Multiplication, Division for floats and integers

addV                             :: Value -> Value -> Either String Value
addV (I x) (I y)                 = Right (I (x+y))
addV (F x) (F y)                 = Right (F (x+y))
addV (F x) (I y)                 = Right (F (x+(fromIntegral(y))))
addV (I x) (F y)                 = Right (F (y+(fromIntegral(x))))
addV _ _                         = Left "Addition of incompatible types."

subV                             :: Value -> Value -> Either String Value
subV (I x) (I y)                 = Right (I (x-y))
subV (F x) (F y)                 = Right (F (x-y))
subV (F x) (I y)                 = Right (F (x-(fromIntegral(y))))
subV (I x) (F y)                 = Right (F ((fromIntegral(x)-y)))
subV _ _                         = Left "Subtraction of incompatible types."


mulV                             :: Value -> Value -> Either String Value 
mulV (I x) (I y)                 = Right (I (x*y))
mulV (F x) (F y)                 = Right (F (x*y))
mulV (F x) (I y)                 = Right (F (x*(fromIntegral(y))))
mulV (I x) (F y)                 = Right (F (y*(fromIntegral(x))))
mulV _ _                         = Left "Multiplication of incompatible types."

divV                             :: Value -> Value -> Either String Value 
divV _ (I 0)                     = Left "Cannot divide by zero."
divV _ (F 0)                     = Left "Cannot divide by zero."
divV (I x) (I y)                 = Right (I (x `div` y))
divV (F x) (F y)                 = Right (F (x/y))
divV (F x) (I y)                 = Right (F (x/(fromIntegral(y))))
divV (I x) (F y)                 = Right (F ((fromIntegral(x))/y))
divV _ _                         = Left "Division of incompatible types."

negV                             :: Value ->
                                    Either String Value
negV x                           = mulV (I (-1)) x
------------------------------------------------------------
-- Boolean function definitions

notV                             :: Value -> Either String Value
notV (B x)                        = Right (B (not x))
notV _                            = Left "Logical NOT on incompatible types"

andV                             :: Value -> Value -> Either String Value
andV (B x) (B y)                 = Right (B (x && y))
andV _ _                         = Left "Logical AND on incompatible types"

orV                              :: Value -> Value -> Either String Value
orV (B x) (B y)                  = Right (B (x || y))
orV  _ _                         = Left "Logical OR on incompatible types"


------------------------------------------------------------
-- Relational function definitions

eqV                              :: Value -> Value -> Either String Value
eqV (I x) (I y)                 = Right (B (x==y))
eqV (F x) (F y)                 = Right (B (x==y))
eqV (F x) (I y)                 = Right (B (x==(fromIntegral(y))))
eqV (I x) (F y)                 = Right (B (y==(fromIntegral(x))))
eqV _ _                         = Left "Equality check of incompatible types."

gtV                              :: Value -> Value -> Either String Value
gtV (I x) (I y)                 = Right (B (x>y))
gtV (F x) (F y)                 = Right (B (x>y))
gtV (F x) (I y)                 = Right (B (x>(fromIntegral(y))))
gtV (I x) (F y)                 = Right (B (y>(fromIntegral(x))))
gtV _ _                         = Left "Order check of incompatible types."

ltV                              :: Value -> Value -> Either String Value
ltV (I x) (I y)                 = Right (B (x<y))
ltV (F x) (F y)                 = Right (B (x<y))
ltV (F x) (I y)                 = Right (B (x<(fromIntegral(y))))
ltV (I x) (F y)                 = Right (B (y<(fromIntegral(x))))
ltV _ _                         = Left "Order check of incompatible types."

------------------------------------------------------------
--Absolute Value function for floats and ints
absV                             :: Value -> Either String Value
absV (I x)                       | x < 0       = Right (I (-x))
                                 | otherwise   = Right (I x)

absV (F x)                       | x < 0       = Right (F (-x))
                                 | otherwise   = Right (F x)
absV _                           = Left "Absolute Value of incompatible type"
--------------------------------------------------------------
--Power function for floats and ints -- Negative exponents and float exponents not supported
--
powV                             :: Value -> Value -> Either String Value
powV (I x) (I y)                 | y > 0       = Right (I (x^y))  
                                 | y < 0       = Right (F ((fromIntegral(x))^(fromIntegral(y))))
                                 | otherwise   = Right (I 1)
    
powV (F x) (I y)                 | y == 0      = Right (I 1)
                                 | otherwise   = Right (F (x^(fromIntegral(y))))
powV _ _                         = Left "Exponent Operation with incompatible types."

--------------------------------------------------------------
--Mod function. Only works with integers. 

modV                             :: Value -> Value -> Either String Value
modV (I x) (I y)                 = Right (I (x `mod` y))
modV _ _                         = Left "Modulo with incompatible types."

                        
