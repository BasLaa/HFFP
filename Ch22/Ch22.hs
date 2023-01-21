-- Reader is a way of stringing functions together
-- when all those functions are awaiting a shared input

-- functor of functions:
-- instance Functor ((->) r) where
    -- fmap = (.)
-- (->) r is the same as r -> in r -> b
-- we lift over (->) r

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> ((->) r) a -> ((-> r) a)

-- reader is the functor of functions
newtype Reader' r a =
    Reader' { runReader' :: r -> a }

instance Functor (Reader' r) where 
    -- fmap :: (a -> b) -> Reader r a -> Reader r b
    -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f (Reader' ra) = 
        Reader' $ \r -> f (ra r) -- is equal to Reader $ (f . ra)

ask :: Reader' a a -- Reader' with some function (a -> a)
ask = Reader' id


myLiftA2 :: Applicative f => (a -> b -> c) 
            -> f a -> f b -> f c 
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader' r a 
asks f = Reader' f

instance Applicative (Reader' r) where
    -- pure :: a -> Reader r a
    -- returns type a, but we don't know type r
    pure a = Reader' $ const a

    -- (<*>) :: f (a -> b) -> f a -> f b
    --  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
    (<*>) (Reader' rab) (Reader' ra) = Reader' $ \r -> rab r (ra r)


instance Monad (Reader' r) where 
    return = pure
    -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b 
    -- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
    (Reader' ra) >>= aRb = Reader' $ \r -> runReader' (aRb $ ra r) r