module Value where

data Value = I Int
  | Dec Float
  deriving Show



addV                             :: Value -> Value -> Value
addV (I x) (I y)                 = I(x+y)
addV (Dec x) (Dec y)             = Dec (x+y)
addV (Dec x) (I y)               = Dec(x+(fromIntegral(y)))
addV (I x) (Dec y)               = Dec(y+(fromIntegral(x))) 
addV _ _                         = undefined

subV                             :: Value -> Value -> Value
subV (I x) (I y)                 = I (x-y)
subV (Dec x) (Dec y)             = Dec (x-y)
subV (Dec x) (I y)               = Dec (x-(fromIntegral(y))) 
subV (I x) (Dec y)               = Dec (y-(fromIntegral(x)))

 
