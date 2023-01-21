-- Chapter 4: Basic datatypes

-- Type level: reading type signatures
-- Term level: values types can take
-- sum type ~ disjunction

-- data Bool = True | False

-- Most numeric types HAVE an instance for Num (typeCLASS)
-- an instance says how that type behaves for the typeclass operators
-- (/) :: Fractional a => a -> a -> a
-- 'Fractional a' is a typeclass constraint, 'a' must implement typeclass Fractional
-- Num is a superclass of Fractional

-- Two tuple is expressed at both type and term level as (,)
-- :info (,)
--  data (,) a b = (,) a b
-- a and b are parameters
-- This is a product type ~ conjunction
-- fst :: (a, b) -> a
-- snd :: (a, b) -> b

module Ch4 where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
