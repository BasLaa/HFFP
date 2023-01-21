-- :t 13
-- 13 :: Num a => a
-- the type of 13 is not yet determined,
-- so it's a constrained polymorphic

-- unconstrained polymorphism = parametric polymorphism

-- -> is also a type constructor, like Bool or (,)
-- the type constructor for functions
-- data (->) a b
-- data (,) a b = (,) a b
-- -> has no data constructor
-- -> is infix and right associative
-- a -> b -> c = a -> (b -> c)
-- the outermost argument, a, will be applied first

-- compiler always gives the most general type it can

-- note that data (->) a b shows that a function takes
-- just one argument, similar to the lambda calculus
-- therefore, we need currying
-- (+) :: Num a => a -> a -> a
-- (+1) :: Num a => a -> a
-- This is partial application

------------- Type inference and declaration --------------
-- Haskell’s type inference is built on an extended
-- version of the Damas-Hindley-Milner type system
