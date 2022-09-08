module Boolean where

    --Boolean type
    data Boolean = T | F
        deriving (Show)

    --Logic or function lor
    lor :: Boolean -> Boolean -> Boolean
    lor T _ = T
    lor _ T = T
    lor _ _ = F

    --Logic and function land
    land :: Boolean -> Boolean -> Boolean
    land F _ = F
    land _ F = F
    land _ _ = T