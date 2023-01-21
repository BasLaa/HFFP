import Data.Char

-- LISTS

-- defined as data [] a = [] | a : [a]
-- [] is the type constructor for lists as well as the empty data constructor
-- : (cons) is an infix operator

-- (:) :: a -> [a] -> [a]
-- pattern matching on data constructor:
-- myHead [] = []
-- myHead (x: _) = x
-- :t myHead = [a] -> a

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt s e
  | s > e = []
  | s < e = s : (eftInt (succ s) e)
  | s == e = [e]

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Enum a, Ord a, Eq a) => a -> a -> [a]
eft s e
  | s > e = []
  | s == e = [e]
  | s < e = s : eft (succ s) e

myWords :: String -> [String]
myWords [] = []
myWords s = if length s == 0 then []
            else word : myWords rest
            where word = takeWhile (/= ' ') s
                  rest = drop 1 (dropWhile  (/= ' ') s)

myLines :: String -> [String]
myLines [] = []
myLines s = if length s == 0 then []
            else word : myLines rest
            where word = takeWhile (/= '\n') s
                  rest = drop 1 (dropWhile  (/= '\n') s)

mySplit :: String -> Char -> [String]
mySplit [] _ = []
mySplit s br = if length s == 0 then []
            else word : mySplit rest br
            where word = takeWhile (/= br) s
                  rest = drop 1 (dropWhile  (/= br) s)

-- map :: (a -> b) -> [a] -> [b]
-- fmap :: Functor f => (a -> b) -> f a -> f
-- fmap is a generalization of map, [] is a functor

itIsMystery xs = map (\x -> elem x "aeiou") xs

-- EXERCISES FILTERING
filt3 :: Integral a => [a] -> [a]
filt3 = filter (\x -> (rem x 3) == 0)

lenfilt :: Integral a => [a] -> Int
lenfilt = length . filt3

myFilter :: String -> [String]
myFilter s = filter (\x -> x /= "a") (words s)


-- ZIPPING LISTS

-- zip :: [a] -> [b] -> [(a, b)]
-- zip [1, 2, 3] [4, 5, 6]
-- [(1,4),(2,5),(3,6)]
-- zip stops when either list ends
-- unzip undoes zip

-- zipwith applies functions requiring two args using the two lists
-- kind of like map for funcs with two args
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

zipp :: [a] -> [b] -> [(a, b)]
zipp [] _ = []
zipp _ [] = []
zipp (x:xs) (y:ys) = (x, y) : zipp xs ys


zippWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zippWith f [] _ = []
zippWith f _ [] = []
zippWith f (x:xs) (y:ys) = f x y : zippWith f xs ys

zippp :: [a] -> [b] -> [(a, b)]
zippp l1 l2 = zippWith (,) l1 l2

cap :: [Char] -> [Char]
cap (x:xs) = toUpper x : xs

reccap :: String -> String
reccap [] = []
reccap (x:xs) = toUpper x : reccap xs

headcap :: String -> Char
headcap = toUpper . head

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = case f x of
  True -> True
  False -> myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem e l = myAny (== e) l

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] =  errorWithoutStackTrace "List.maximumBy: empty list"
myMaximumBy _ [x] = x
myMaximumBy c (x:xs) = case c x te of
  GT -> x
  LT -> te
  Eq -> te
  where te = myMaximumBy c xs
