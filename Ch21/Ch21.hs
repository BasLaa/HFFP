-- Traversable is commonly described as a way to traverse a data structure
-- accumalating applicative contexts along the way

-- class (Functor t, Foldable t) => Traversable t where
--     traverse :: Applicative f => (a -> f b)
--                 -> t a
--                 -> f (t b)
--     traverse f = sequenceA . fmap f

-- sequenceA :: Applicative f
--            => t (f a) -> f (t a) 
-- sequenceA = traverse id

--   {-# MINIMAL traverse | sequenceA #-}
-- traverse and sequenceA can be defined in terms of each other


-- sequenceA
-- Flips two structures: t (f a) -> f (t a)
-- sequenceA [Just 1] -> Just [1]
-- sequenceA [Just 1, Nothing] -> Nothing


-- traverse
-- traverse :: (Applicative f, Traversable t)
--              => (a -> f b) -> t a -> f (t b)
-- similar to flip bind (=<<) :: (a -> m b) -> m a -> m b
-- we are mapping a function of values in a structure, 
-- creating more structure, then flipping it
-- traverse f = sequenceA . fmap f