module Apply where 

import CalcParser


apply :: BinOp -> (Value -> Value -> Either String Value)
apply (AO Add)     = addV
apply (AO Sub)     = subV
apply (AO Mul)     = mulV
apply (AO Div)     = divV
apply (BO And)     = andV
apply (BO Or)      = orV
apply (RO Less)    = ltV
apply (RO Greater) = gtV
apply (RO Equal)   = eqV

-- | function that performs addition. It pattern matches based upon
-- the different types of values that are possible
addV                             :: Value -> Value -> Either String Value
addV (I x) (I y)                 = Right (I (x+y))
addV (D x) (D y)                 = Right (D (x+y))
addV (D x) (I y)                 = Right (D (x+(fromIntegral(y))))
addV (I x) (D y)                 = Right (D (y+(fromIntegral(x))))
addV _ _                         = Left "Addition of incompatible types."

-- | function that performs subtraction. It pattern matches based upon
-- the different types of values that are possible
subV                             :: Value -> Value -> Either String Value
subV (I x) (I y)                 = Right (I (x-y))
subV (D x) (D y)                 = Right (D (x-y))
subV (D x) (I y)                 = Right (D (x-(fromIntegral(y))))
subV (I x) (D y)                 = Right (D ((fromIntegral(x)-y)))
subV _ _                         = Left "Subtraction of incompatible types."

-- | function that performs multiplication. It pattern matches based upon
-- the different types of values that are possible
mulV                             :: Value -> Value -> Either String Value 
mulV (I x) (I y)                 = Right (I (x*y))
mulV (D x) (D y)                 = Right (D (x*y))
mulV (D x) (I y)                 = Right (D (x*(fromIntegral(y))))
mulV (I x) (D y)                 = Right (D (y*(fromIntegral(x))))
mulV _ _                         = Left "Multiplication of incompatible types."

-- | function that performs division. It pattern matches based upon
-- the different types of values that are possible
divV                             :: Value -> Value -> Either String Value 
divV _ (I 0)                     = Left "Cannot divide by zero."
divV _ (D 0)                     = Left "Cannot divide by zero."
divV (I x) (I y)                 = Right (I (x `div` y))
divV (D x) (D y)                 = Right (D (x/y))
divV (D x) (I y)                 = Right (D (x/(fromIntegral(y))))
divV (I x) (D y)                 = Right (D ((fromIntegral(x))/y))
divV _ _                         = Left "Division of incompatible types."

------------------------------------------------------------
-- Boolean function definitions


-- | function that performs the logical NOT operation. It pattern matches based upon
-- the different types of values that are possible
notV                             :: Value -> Either String Value
notV (B x)                        = Right (B (not x))
notV _                            = Left "Logical NOT on incompatible types"

-- | function that performs the logical AND operation. It pattern matches based upon
-- the different types of values that are possible
andV                             :: Value -> Value -> Either String Value
andV (B x) (B y)                 = Right (B (x && y))
andV _ _                         = Left "Logical AND on incompatible types"

-- | function that performs the logical OR operation. It pattern matches based upon
-- the different types of values that are possible
orV                              :: Value -> Value -> Either String Value
orV (B x) (B y)                  = Right (B (x || y))
orV  _ _                         = Left "Logical OR on incompatible types"


------------------------------------------------------------
-- Relational function definitions


-- | function that performs the equivalence operation. It pattern matches based upon
-- the different types of values that are possible
eqV                              :: Value -> Value -> Either String Value
eqV (I x) (I y)                 = Right (B (x==y))
eqV (D x) (D y)                 = Right (B (x==y))
eqV (D x) (I y)                 = Right (B (x==(fromIntegral(y))))
eqV (I x) (D y)                 = Right (B (y==(fromIntegral(x))))
eqV _ _                         = Left "Equality check of incompatible types."

-- | function that performs the greater-than operation. It pattern matches based upon
-- the different types of values that are possible
gtV                              :: Value -> Value -> Either String Value
gtV (I x) (I y)                 = Right (B (x>y))
gtV (D x) (D y)                 = Right (B (x>y))
gtV (D x) (I y)                 = Right (B (x>(fromIntegral(y))))
gtV (I x) (D y)                 = Right (B (y>(fromIntegral(x))))
gtV _ _                         = Left "Order check of incompatible types."

-- | function that performs less-than operation. It pattern matches based upon
-- the different types of values that are possible
ltV                              :: Value -> Value -> Either String Value
ltV (I x) (I y)                 = Right (B (x<y))
ltV (D x) (D y)                 = Right (B (x<y))
ltV (D x) (I y)                 = Right (B (x<(fromIntegral(y))))
ltV (I x) (D y)                 = Right (B (y<(fromIntegral(x))))
ltV _ _                         = Left "Order check of incompatible types."

