module Tipos where
    -- Known typeclasses: Num, Integral, Fractional
    -- Known primetive types: Bool, Char, Float, Double, Integer
    -- Known compound types: STRING [Char], TUPLE (a,b), FUNCTION (Int -> Int)
    --data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
        deriving (Show)

    nextDay :: Weekday -> Weekday
    nextDay Mon = Tue
    nextDay Tue = Wed
    nextDay Wed = Thu
    nextDay Thu = Fri
    nextDay Fri = Sat
    nextDay Sat = Sun
    nextDay Sun = Mon

    nextWorkingDay :: Weekday -> Weekday
    nextWorkingDay Mon = Tue
    nextWorkingDay Tue = Wed
    nextWorkingDay Wed = Thu
    nextWorkingDay Thu = Fri
    nextWorkingDay Fri = Mon
    nextWorkingDay Sat = Mon
    nextWorkingDay Sun = Mon

    nextWorkingDay' :: Weekday -> Weekday
    nextWorkingDay' Mon = Tue
    nextWorkingDay' Tue = Wed
    nextWorkingDay' Wed = Thu
    nextWorkingDay' Thu = Fri
    nextWorkingDay' _ = Mon

    nextWorkingDay'' :: Weekday -> Weekday
    nextWorkingDay'' Fri = Mon
    nextWorkingDay'' Sat = Mon
    nextWorkingDay'' Sun = Mon
    nextWorkingDay'' d = nextDay d