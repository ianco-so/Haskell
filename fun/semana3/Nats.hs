module Nats where

data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus n Zero = n
plus Zero m = m
plus (Succ n) m = Succ (plus n m)

instance (Show Nat) where
    show Zero = "0"
    show (Succ n) = (show 1) ++ (show '+' ) ++ (show n)