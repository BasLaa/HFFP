-- FOLDING LISTS
-- The generalization of a fold is a catamorphism : deconstructing data, reducing structure
-- fold has been generalized to the Foldable typeclass

-- RIGHT ASSOCIATIVE
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z xs =
--  case xs of
--    [] -> z
--    (x:xs) -> f x (foldr f z xs)
-- folding a function over list of type a with an initial value of type b
-- foldr adds z to the end of the list first

-- LEFT ASSOCIATIVE
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs
-- foldl adds the accumalator to the start of the list

-- We write folds by
-- 1. Considering the initial value (often the function identity)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combs :: String -> String -> [(Char, Char, Char)]
combs s v = [(x, y, z) | x <- s, y <- v, z <- s]

combsp :: String -> String -> [(Char, Char, Char)]
combsp s v = [(x, y, z) | x <- s, x == 'p', y <- v, z <- s]

-- seekritFunc :: String -> Int
-- seekritFunc x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (\a b -> if a then True else b) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> if f a then True else b) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\a b -> if a == e then True else b) False

myReverse :: [a] -> [a]
myReverse xs = foldr (\a b -> b ++ [a]) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\a b -> [f a] ++ b) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then [a] ++ b else b) []

squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp (x:xs) = foldr (inncomp) x (x:xs) where
  inncomp a b = case cmp a b of
    LT -> b
    GT -> a
    EQ -> b
