-- FUNCTORS
-- we want to apply a function to a value 'inside' a structure
-- while leaving the structure intact
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

import Test.QuickCheck
import Test.QuickCheck.Function

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

-- functor laws:
-- Identity:
-- fmap id == id
-- composition:
-- fmap f . fmap g == fmap (f . g)

-- Common functors:
-- Maybe
-- []
-- (,)

a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = fmap (*2) (\x -> x - 2)


-- With some types, fmap only applies onto one argument
-- (,): kind * -> * -> *
data Two a b = Two a b deriving (Eq, Show)
-- either: kind * -> * -> *
data Or a b = First a | Second b deriving (Eq, Show)

-- but functor wants kind * -> *
-- Here (Two a) and (Or a) are the functorial structure
-- We cannot touch it, so we can only apply to b
instance Functor (Two a) where 
    fmap f (Two a b) = Two a (f b) 
instance Functor (Or a) where 
    fmap f (First a) = First a  -- cannot touch a
    fmap f (Second b) = Second (f b)

-- quickcheck for checking functor law
functorCompose' :: (Eq (f c), Functor f) => 
                    f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

type IntToInt = Fun Int Int

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool
type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- Test.QuickCheck.arbitrary
        return (Identity a)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- Test.QuickCheck.arbitrary
        return (Pair a a)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- Test.QuickCheck.arbitrary
        b <- Test.QuickCheck.arbitrary
        c <- Test.QuickCheck.arbitrary
        d <- Test.QuickCheck.arbitrary
        return (Four a b c d)

--- 

data Sum a b = First' b | Second' a
instance Functor (Sum e) where 
    fmap f (First' a) = First' (f a) 
    fmap f (Second' b) = Second' b

data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

--- 

data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    -- fmap f (Read g) = fmap $ \x -> (g x)