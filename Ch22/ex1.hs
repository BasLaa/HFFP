import Control.Applicative
import Data.Char

boop = (*2)
doop = (+10)

-- first applies doop, then boop
bip :: Integer -> Integer
bip = boop . doop

-- composition can also be written as
bloop :: Integer -> Integer 
bloop = fmap boop doop
-- we are fmapping a function over another function
-- the context is a partially applied context
-- fmap :: (a -> b) -> f a -> f b
-- f here is (a - >)

-- Here, the behaviour is different
-- the argument will be passed to boop and doop
-- and then combined via (+)
bbop :: Integer -> Integer 
bbop = (+) <$> boop <*> doop
-- (+) <$> (*2) == (+). (*2) :: a -> a -> a

duwop :: Integer -> Integer 
duwop = liftA2 (+) boop doop

------- Mini exercise

cap :: [Char] -> [Char]
cap xs = map toUpper xs 

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap 

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
    a <- cap
    b <- rev
    return (a, b)

-- (>>=) :: m a -> (a -> m b) -> m b
-- (<*>) :: f (a -> b) -> f a -> f b
tupledM2 :: [Char] -> ([Char], [Char])
tupledM2 = cap >>= (\x -> rev >>= (\y -> return (x, y)))