module Boolean where

    --Boolean type
    data Boolean = T | F
        deriving (Show)

    --Logic or function lor
    lor :: Boolean -> Boolean -> Boolean
    lor F F = F
    lor _ _ = T

    --Logic and function land
    land :: Boolean -> Boolean -> Boolean
    land T T = T
    land _ _ = F