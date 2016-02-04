module Value where

data Value = I Int
  | F Float
  deriving Show



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

absV                             :: Value -> Value
absV (I x)                       | x < 0       = (I (-x))
                                 | otherwise   = (I x)

absV (F x)                     | x < 0       = (F (-x))
                                 | otherwise   = (F x)
