import Data.Char (toUpper)
------------------ ALGEBRAIC DATATYPES ---------------------
-- data Bool = False | True
-- Bool is type constructor, False and True are data constructors
-- They are nullary constructors
-- This is a sum type

-- types are used in typeclass declarations and signatures
-- they are static and resolve at compile time
-- They are also usd for type aliases: String = [Char]

-- data constructors actually construct values you use at runtime
-- they define a way of building a type

-- Bool is actuàlly a type constant as it takes no args
-- similarly, True and False are data constants

-- Kinds are the types to describe types: * -> *
-- a fully applied type is represented as *

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini $ Price 14000
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

-- won't work on Plane, non-exhaustive
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- A type can be thought of as an enumeration
-- of constructors that have zero or more argument


-- NEWTYPE
-- we can define only unary datatypes with newtype
newtype Goats = Goats Int deriving (Eq, Show)
-- to the compiler, there is no difference
-- newtypes are similar to type aliases
-- BUT, we can define typeclass instances for newtypes
-- that their underlying types dont have

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- keyword type is used for type synonyms

-- SUM AND PRODUCT TYPES
-- You can easily find the cardinality of types

-- RECORD SYNTAX
-- data Person =
--   Person { name :: String
--           , age :: Int }
--           deriving (Eq, Show)
-- using this syntax, you can directly access the records
-- let papu = Person "Papu" 5
-- age papu -> 5
-- name papu -> "Papu"

-- in algebra, distributivity of sums and products hold: 2 * (3 + 4) = 2 * 3 + 2 * 4
-- in Haskell, we bring a type declaration into normal form by writing it as sum of products
data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
           , lang :: ProgLang }
  deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = x, lang = y} | x <- allOperatingSystems, y <- allLanguages]


-- Higher kinded datatypes: * -> * -> *
-- This is awaiting two arguments to be fully applied

-- BINARY TREES
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
          => a
          -> BinaryTree a
          -> BinaryTree a

insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b

mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
  1
  (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
  2
  (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a] -- curr left right
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a] -- left curr right
inorder Leaf = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

postorder :: BinaryTree a -> [a] -- left right curr
postorder Leaf = []
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

foldTree :: (a -> b -> b)
          -> b
          -> BinaryTree a
          -> b
foldTree f acc tree = foldr f acc (inorder tree)

testFold :: IO ()
testFold =
  if foldTree (+) 0 testTree == 6
    then putStrLn "good"
  else putStrLn "broken"



-- CHAPTER EXERCISES

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf l@(a:as) (b:bs) = if a == b
  then isSubseqOf as bs
  else isSubseqOf l bs

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs
