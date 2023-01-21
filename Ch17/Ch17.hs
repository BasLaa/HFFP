-- an Applicative is a monoidal functor
-- Both the function and the value are lifted over structure
-- import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

class Functor f => Applicative' f where
    pure' :: a -> f a
    (<*>>) :: f (a -> b) -> f a -> f b

-- Notice that applicative requires functor -> superclass
-- pure lifts a into applicative structure f
-- <*> is called apply or ap: generalized function application

-- applicative also comes with functions:
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- very similar to fmap (<$>) but with Applicative

-- in fact, we have that
-- fmap f x = pure f <*> x

-- In Applicative, we also may need to change the structure f
-- We can change the structure itself by monoids

-- For (,) a b, (,) a is the structure f
-- fmap (+1) ("blah", 0) -> ("blah", 1)

-- but ("Woo", (+1)) <*> ("Hoo", 2) -> ("Woohoo", 3)
-- so the structure is changed by "Woo" <> "Hoo"
-- but the b part of tuple is done by function

instance Monoid a => Applicative' ((,) a) where
    pure' x = (mempty, x)
    (u, f) <*>> (v, x) = (u `mappend` v, f x)

-- Applicative in use

------------ LISTS ------------
-- Here we have a list of functions and a list of values
-- each function is mapped onto each value
-- [(+1), (*2)] <*> [2, 4] = [3, 4, 5, 8]

-- Example:
-- (+) <$> [1, 2] <*> [3, 5]     recall <$> is fmap
-- this gives: [(+1), (+2)] <*> [3, 5] = [4, 5, 6, 7]
-- same as liftA2 (+) [1, 2] [3, 5]

-- looks up element in list of tuples
-- only looks at the first element and returns second (key, elem)
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- Exercises
added :: Maybe Integer 
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

--- 

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer) 
tupled = (,) <$> y <*> z

---

-- elemIndex returns index of first occurrence

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int 
max' = max

maxed :: Maybe Int 
maxed = max' <$> x <*> y'

------------- IDENTITY --------------
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative' Identity where
    pure' = Identity
    (Identity f) <*>> x  = fmap f x


-------------- CONSTANT ------------

newtype Constant a b = 
    Constant { getConstant :: a } 
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
    fmap f (Constant b) = Constant b

instance Monoid a => Applicative (Constant a) where
    pure b = Constant mempty 
    (Constant b) <*> (Constant c) = Constant $ b <> c


--------------- MAYBE -----------------
validateLength :: Int -> String -> Maybe String 
validateLength maxLen s =
    if (length s) > maxLen 
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name 
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address 
mkAddress a = fmap Address $ validateLength 100 a


data Person = Person Name Address deriving (Eq, Show)
mkPerson :: String-> String -> Maybe Person 
mkPerson n a = Person <$> mkName n  <*> mkAddress a


-------- Applicative laws --------

-- 1. Identity:
--    pure id <*> v = v

-- 2. Composition
--    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- 3. Homomorphism
-- pure f <*> pure x = pure (f x)

-- Integerchange
-- u <*> pure y = pure ($ y) <*> u
-- see that:
-- u :: f (a -> b)
-- :t ($ y) = (a -> b) -> b
-- so in   pure ($ y) <*> u
--         (a -> b) -> b is the function x -> y
--         and u will be f (a -> b)

---------- Exercise List Applicative ------------
data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a 
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-- fmap (+1) [1, 2] -> [2, 3]
instance Functor List where 
    fmap f Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

-- [(+1), (+2)] <*> [1, 4] -> [2, 5, 3, 6]
instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fl) <*> v = (fmap f v) `append` (fl <*> v)

------------ Ziplist monoid -----------
-- The list monoid works as [1, 2] <> [3, 4] -> [1, 2, 3, 4]
-- but we could also think of 
-- [1, 2] <> [3, 4] -> [1 <> 3, 2 <> 4]

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

-- Like in lists
instance Functor ZipList' where 
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Monoid a => Monoid (ZipList' a) where
    mempty = pure mempty 
    mappend = liftA2 mappend

-- Ziplist [1, 2, 3] <> Ziplist [1, 2, 3] = [2, 4, 6]
instance Applicative ZipList' where 
    pure x = ZipList' x
    ZipList' Nil <*> ZipList' l = ZipList' Nil
    ZipList' _ <*> ZipList' Nil = ZipList' Nil
    ZipList' (Cons f fl) <*> ZipList' l = 
        ZipList' ((fmap f l) `append` (ZipList' fl <*> ZipList' l))

