module Quinto where
    
    ifThenElse :: Bool -> a -> a -> a
    ifThenElse True  x _ = x
    ifThenElse False _ y = y

    x :: Int
    x = 10