-- a monoid is a binary associative operation with identity
-- Typeclasses give us a way to organize common functionality
-- like numbers, but also monoids: 
--      types that have binary function with associativity and identity
import Data.Monoid
import Data.Semigroup
import Data.List.NonEmpty

class Monoid_ m where
    mempty_ :: m     -- identity
    mappend_ :: m -> m -> m  -- the binary operation
    mconcat_ :: [m] -> m
    mconcat_ = foldr mappend_ mempty_

-- note mappend = (<>)
-- <> is infix

-- EXAMPLES

-- Lists:
-- mempty = []
-- mappend = (++)
-- mconcat = foldr (++) []

-- Integers form a monoid under summation and multiplication
-- There is no general one, must specify:
-- mappend (Sum 1) (Sum 2)

-- newtype usage: only unary data constructor
--      signaling intent:
--      type safety
--      different typeclasses

-- we use newtype to wrap (Sum a) or (Product a)
-- around integers to distinguish the monoid

-- Monoid laws:

-- identity
-- mappend mempty x = x
-- mappend x mempty = x

-- associativity
-- mappend a (mappend b c) = mappend (mappend a b) c
-- so, a <> (b <> c) = (a <> b) <> c

-- many sets have multiple monoids, we use newtypes to dinstinguish
-- Maybe:
-- First (Just 1) <> First (Just 2) = 1
-- Last (Just 1) <> Last (Just 2) = 2

-- Exercise:
data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  m <> Nada = m
  Nada <> m = m
  (Only x) <> (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada
  mappend m        Nada     = m
  mappend Nada     m        = m
  mappend (Only x) (Only y) = Only (mappend x y)


type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = mconcat [e, adv, noun, adj]

-- SEMIGROUP
-- Semigroup is binary associative operation without identity:
-- class Semigroup_ a where
--     (<>) :: a -> a -> a
-- where (a <> b) <> c = a <> (b <> c)

-- Example: NonEmpty: non-empty list has no identity
data NonEmpty a = a :| [a]
  deriving (Eq, Ord, Show)
-- :| is a data constructor like Maybe, only infix

-- There is an inverse relation between the number
-- of operations a type permits and the number of types
-- that can satisfy it

-- EXERCISES

-- SEMIGROUP
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where 
    Trivial <> Trivial = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend Trivial Trivial = Trivial

---

newtype Identity a = Identity a

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity a) (Identity b) = Identity (mappend a b)

---

data Two a b = Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend (Two a b) (Two a' b') = Two (a `mappend` a') (b `mappend` b')

---

-- three, four...

---

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Monoid BoolConj where
    mempty = BoolConj False
    mappend = (<>)

---

-- BoolDisj...

---

data Or a b = Fst a | Snd b

instance Semigroup (Or a b) where
    (<>) (Fst a) (Snd b) = Snd b
    (<>) (Snd b) (Fst a) = Snd b
    (<>) (Fst a) (Fst b) = Fst a

---

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance Monoid b => Monoid (Combine a b) where
    mempty = Combine (\x -> mempty)
    mappend = (<>)

---

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid a => Monoid (Comp a) where
    mempty = Comp (\x -> mempty)
    mappend = (<>)

---

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
    Mem f <> Mem g = Mem (\s -> (s, a)) 

-- instance Monoid a => Monoid (Mem s a) where 
--     mempty = undefined
--     mappend = undefined