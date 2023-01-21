-- A monad is an applicative functor
-- with unique behavior making it more powerful

-- Applicative is a superclass of Monad
-- Just like Functor is a superclass of Applicative

-- IO is most used monad

import Control.Monad (join, liftM)
import Control.Applicative (liftA2)

-- class Applicative m => Monad' m where
    -- (>>=) :: m a -> (a -> m b) -> m b
    -- (>>) :: m a -> m b -> m b
    -- return' :: a -> m a
-- You only need (>>=) for a minimal monad

-- We can write fmap as monadic operations
fmap' f xs = xs >>= return . f


-- return is the same as pure
-- it just wraps a value in structure

-- (>>) is the sequencing operator
-- sequences two actions and discards resulting
-- value of the first action
-- This also exists for applicatives:
-- (*>) :: Applicative f => f a -> f b -> f b


-- (>>=) is the bind operator:
-- (>>=) :: m a -> (a -> m b) -> m b
-- what bind does is bypass the structure m surrounding a
-- and use it for the function (a -> m b)
-- which will give back a wrapped in two layers of structure
-- Finally, it flattens the structure using 'join'

-- 'join' is a generalized concept of concat
-- concat :: [[a]] -> [a]
-- join :: m (m a) -> m a


-- we can write bind in terms of fmap and join
-- recall fmap :: (a -> b) -> f a -> f b
-- but we can write b == f b
-- fmap :: (a -> f b) -> f a -> f (f b)

bind :: Monad m => (a -> m b) -> m a -> m b 
bind f a = join $ fmap f a

-- do syntax
-- (*>) and (>>) are sequencing functions
-- putStrLn "Hello, " >> putStrLn "World" =
-- Hello,
-- World

-- :t putStrLn = String -> IO ()

-- same as do
    -- putStrLn "hello, "
    -- putStrLn "world"


-------------- LIST MONAD --------------
-- (>>=) :: [a] -> (a -> [b]) -> [b]
-- return :: a -> [a]

twiceWhenEven :: [Integer] -> [Integer] 
twiceWhenEven xs = do
    x <- xs 
    if even x
        then [x*x, x*x] 
    else []

-- here xs is (m a) and if ... then ... is (a -> m b)


-------------- MAYBE MONAD ----------------
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- return :: a -> Maybe a

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String 
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int 
noNegative n 
    | n >= 0 = Just n
    | otherwise = Nothing

-- Cow named Bess has to weigh less than 500
weightCheck :: Cow -> Maybe Cow 
weightCheck c = let 
    w = weight c
    n = name c 
    in 
        if n == "Bess" && w > 499 
            then Nothing
        else Just c


mkSphericalCow' :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow' name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight' 
    weightCheck (Cow nammy agey weighty)


------------- EITHER MONAD --------------
-- (>>=) :: Either e a -> (a -> Either e b) -> Either e b
-- return :: a -> Either e a

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where 
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where 
    pure = Second
    (<*>) (First f) x = First f
    (<*>) (Second f) x = fmap f x

instance Monad (Sum a) where 
    return = pure
    (>>=) (First a) f = First a
    (>>=) (Second b) f = f b




--------------- Monad laws -----------------

-- IDENTITY
-- m >>= return = m
-- return x >>= f = f x


-- ASSOCIATIVITY
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)



----------------- Exercises -----------------

data Nope a = NopeDotJpg

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    m >>= f = NopeDotJpg


---

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where 
    pure = Identity
    (Identity f) <*> x = fmap f x

instance Monad Identity where 
    return = pure
    (Identity a) >>= f = f a


---


data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a 
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where 
    fmap f Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

-- [(+1), (+2)] <*> [1, 4] -> [2, 5, 3, 6]
instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fl) <*> v = (fmap f v) `append` (fl <*> v)

-- >>= :: [a] -> (a -> [b]) -> [b]
instance Monad List where
    return = pure
    Nil >>= _ = Nil
    (Cons a l) >>= f = (f a) `append` (l >>= f)


---


j :: Monad m => m (m a) -> m a
j v = v >>= id


l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = fmap f m


l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 = liftA2
l2 f m1 m2 = fmap f m1 <*> m2


a :: Monad m => m a -> m (a -> b) -> m b
a m f = f <*> m


meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = fmap (:) (f x) <*> (meh xs f)

flipType :: Monad m => [m a] -> m [a]
flipType l = meh l id