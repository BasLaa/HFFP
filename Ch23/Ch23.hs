-- STATE

-- Using the state monad gives state which:
--   - Doesn't need IO
--   - Limited to state container
--   - explicit in function types

newtype State' s a = State' { runState :: s -> (a, s) }
-- so, the State data constructor takes a function
-- of the form s -> (a, s), and creates state

-- then runState takes a State s a and returns (s -> (a, s))


newtype Moi s a = 
    Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    -- fmap :: (a -> b) -> Moi s a -> Moi s b 
    fmap f (Moi g) = Moi $ \s -> (f (fst (g s)), s)

instance Applicative (Moi s) where 
    -- pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)
    -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \s -> ((fst (f s)) (fst (g s)), s)

instance Monad (Moi s) where
    return = pure
    -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b 
    (>>=) (Moi f) g = Moi $ \s -> runMoi (g (fst (f s))) s


----------- CHAPTER EXERCISES ------------
get :: Moi s s 
get = Moi $ \s -> (s, s)

---
put :: s -> Moi s ()
put s = Moi $ \t -> ((), s)

---
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd (sa s)

---
eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

---

modify :: (s -> s) -> Moi s () 
modify f = Moi $ \s -> ((), f s)