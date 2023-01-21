-- applying a function is binding values to parameters

-- anonymous functions are written as (\x -> x * 3)
-- often used for passing functions as arguments

-- Pattern matching
-- isItTwo :: Int -> Bool
-- isItTwo 2 = True
-- isItTwo _ = False

-- Expanding data constructors through pattern matching:
-- recall the data constructor for tuple: (,) a b
-- first (a, b) = a
-- first tup = fst tup
-- fst built in

-- We can write if-statements as case-statements
-- if x + 1 == 1 then "Hello" else "Bye"
-- case x + 1 == 1 of
--  True -> "Hello"
--  False -> "Bye"

-- guard blocks
-- Abs x
--  | x < 0 = (-x)
--  | otherwise = x

-- COMPOSITION
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (f . g) x = f (g x)
-- g is applied to x first, then the the
-- negate . sum $ [1, 2, 3, 4, 5]
-- or (negate . sum) [1, 2, 3]
-- recall that $ indicates low application precedence
-- we use it to apply . before sum
-- sum has precedence 10, while . has 9, so if we don't have $, we would get negate . 15
