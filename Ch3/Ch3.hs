-- Prefixing infix functions is done as (+) 1 2
-- Get info on expression with :i, type with :t
-- load module with :l, unload :m
-- String :: [char]
-- This is a type alias

-- Top-level vs local definition

-- area d = pi * (r * r)
-- Here d for r is not in scope
-- r = d / 2

-- ++ appends lists
-- concat flattens list of lists

-- Other functions: cons (:), take _ , drop _,  head, tail, !! (find by index),

module Ch3
    (
    ) where

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! (x-1)
