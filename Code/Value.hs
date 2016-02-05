module Value where



data Value = I Int
  | F Float
  | B Bool
  deriving Show


--Addition, Subtraction, Multiplication, Division for floats and integers

addV                             :: Value -> Value -> Value
addV (I x) (I y)                 = I(x+y)
addV (F x) (F y)                 = F (x+y)
addV (F x) (I y)                 = F(x+(fromIntegral(y)))
addV (I x) (F y)                 = F(y+(fromIntegral(x))) 
addV _ _                         = undefined

subV                             :: Value -> Value -> Value
subV (I x) (I y)                 = I (x-y)
subV (F x) (F y)                 = F (x-y)
subV (F x) (I y)                 = F (x-(fromIntegral(y))) 
subV (I x) (F y)                 = F ((fromIntegral(x)-y))


mulV                             :: Value -> Value -> Value 
mulV (I x) (I y)                 = I (x*y)
mulV (F x) (F y)                 = F (x*y)
mulV (F x) (I y)                 = F (x*(fromIntegral(y))) 
mulV (I x) (F y)                 = F (y*(fromIntegral(x)))

divV                             :: Value -> Value -> Value 
divV (I x) (I y)                 = I (x `div` y)
divV (F x) (F y)                 = F (x/y)
divV (F x) (I y)                 = F (x/(fromIntegral(y))) 
divV (I x) (F y)                 = F ((fromIntegral(x))/y) 
------------------------------------------------------------
-- Boolean function definitions

notV                             :: Value -> Value
notV x                           = B (!x)
not _                            = undefined

andV                             :: Value -> Value -> Value
andV (B x) (B y)                 = B (x && y)
andV _ _                         = undefined

orV                              :: Value -> Value -> Value
orV (B x) (B y)                  = B (x || y)
orV  _ _                         = undefined


------------------------------------------------------------
--Absolute Value function for floats and ints
absV                             :: Value -> Value
absV (I x)                       | x < 0       = (I (-x))
                                 | otherwise   = (I x)

absV (F x)                       | x < 0       = (F (-x))
                                 | otherwise   = (F x)

--------------------------------------------------------------
--Power function for floats and ints -- Negative exponents and float exponents not supported
powV                             :: Value -> Value -> Value
powV (I x) (I y)                 | y > 0       =  (I (x^y))  
                                 | y < 0       =  (F ((fromIntegral(x))^(fromIntegral(y))))
                                 | otherwise   =  (I 1)
    
powV (F x) (I y)                 | y == 0      =  (I 1)
                                 | otherwise   =  (F (x^(fromIntegral(y))))


--------------------------------------------------------------
--Mod function. Only works with integers. 

modV                             :: Value -> Value -> Value
modV (I x) (I y)                 = I(x `mod` y)


                        
