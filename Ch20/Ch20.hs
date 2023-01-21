-- Foldable: typeclass
-- class of data structures that can be folded to summary value

-- class Foldable t where
    -- {-# MINIMAL foldMap | foldr #- }

-- :t Foldable
-- class Foldable (t :: * -> *) where..
-- requires higher kinded type

-- class Foldable' (t :: * -> *) where 
--     fold' :: Monoid m => t m -> m 
--     foldMap' :: Monoid m => (a -> m) -> t a -> m

-- the element inside the foldable should have
-- a monoid defined for them for fold

-- for foldMap, each element is first mapped to a monoid
-- (a -> m)

-- example: foldMap Product [1, 2, 3, 4] = 24

-- foldr (+) 0 [1..5]
-- implicitly, (+) maps the integers onto the sum monoid

-- fold [1..5] will give an error: no monoid
import Data.Monoid

data Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

-- other instances: Maybe, Tuple


-- DERIVED OPERATIONS

-- toList :: t a -> [a]
-- toList (Just 1) -> [1]
-- map toList [Just 1, Just 2] -> [[1], [2]]

-- toList (1, 2) -> [2]
-- on tuple, operations only apply to the second element
-- because we have instance Foldable (,) a where ...


-- null :: t a -> Bool
-- null (Left 3) -> True
-- null [] -> True
-- null (1, 2) -> True


-- length :: t a -> Int
-- length (1, 2) -> 1
-- length [(1, 2), (3, 4)] -> 2


-- elem :: Eq a => a -> t a -> Bool
-- elem 2 (Just 3) -> False
-- elem True (Left True) -> False
-- elem True (Right True) -> True
-- fmap (elem 3) [Right 1, Right 3] -> [False, True]


-- maximum :: Ord a => t a -> a
-- minimum :: Ord a => t a -> a
-- must be non-empty structure t
-- maximum "bas" -> s
-- minimum "bas" -> a


-- class Foldable' (t :: * -> *) where 
--     fold' :: Monoid m => t m -> m 
--     foldMap' :: Monoid m => (a -> m) -> t a -> m

-- foldr :: (a -> b -> b) -> b -> t a -> b

-- EXERCISES

sum' :: (Foldable t, Num a) => t a -> a
sum' t = foldr (+) 0 t

product' :: (Foldable t, Num a) => t a -> a
product' t = foldr (*) 1 t

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e t = any ((==) e) t

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' t  = foldr maybeMin Nothing t where
    maybeMin a Nothing = Just a
    maybeMin a (Just b) = Just (min a b)

null' :: (Foldable t) => t a -> Bool
null' t = foldr (\_ _ -> False) True t

length' :: (Foldable t) => t a -> Int
length' t = foldr (\acc x -> x+1) 0 t

toList' :: (Foldable t) => t a -> [a]
toList' t = foldr (\acc x -> acc : x) [] t

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' m t = foldr (\x acc -> (m x) <> acc) mempty t


-----------------------------------------

data Constant a b = Constant b

instance Foldable (Constant a) where
    foldr f x (Constant b) = f b x
    foldMap f (Constant b) = f b 

---

data Two a b = Two a b

instance Foldable (Two a) where
    foldr f x (Two a b) = f b x
    foldMap f (Two a b) = f b 

---

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldr f x (Three a b c) = f c x
    foldMap f (Three a b c) = f c

---

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldr f x (Three' a b1 b2) = f b2 (f b1 x)
    foldMap f (Three' a b1 b2) = (f b1) <> (f b2)

---

filterF :: ( Applicative f, Foldable t, Monoid (f a)) 
            => (a -> Bool) -> t a -> f a
filterF f t = foldMap (\a -> if (f a) then pure a else mempty)
