-- Typeclasses allows generalization over a set of types
-- to define a set of features for those types (equality, +, ordering...)
-- Similar to interfaces in some ways
-- Typeclasses have a 'hierarchy'
-- a type has an instance of a typeclass if it defines its functions

-- example: Eq is a typeclass implementing (==) and (/=)

-- For any new datatype, we can automatically derive instances for:
-- Eq, Ord, Enum, Bounded, Read, Show

-- get info on typeclasses like
-- :info Enum
-- class Enum a where
-- succ :: a -> a
-- ...

module Ch6 where

ones :: Integral b => b -> b
ones x = snd (divMod x 10)
