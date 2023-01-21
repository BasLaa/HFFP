-- data Either a b = Left a | Right b
-- we use left as the error constructor

-- data constructors are functions:
-- * -> * is like a -> b
-- we can use them as functions:
--  fmap Just [1, 2, 3]

notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else (Just s)

replaceThe :: String -> String
replaceThe s = unwords . map (\x -> if notThe x == Nothing then "a" else x) $ (words s)

vowels = "ieoua"

startsVowel :: String -> Bool
startsVowel (x:xs) = x `elem` vowels

isVowel :: Char -> Bool
isVowel = flip elem vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel [x] = 0
countTheBeforeVowel (x:xs) =
  case (words (x:xs) !! 0) == "the" of
    True -> if (startsVowel (words (x:xs) !! 1))
              then 1 + countTheBeforeVowel xs
              else countTheBeforeVowel xs
    False -> countTheBeforeVowel xs

countVowels :: String -> Int
countVowels = length . filter (== True) . fmap isVowel

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str = let v = countVowels str in
   if v > ((length str) - v) then Nothing else (Just (Word' str))


lefts' :: [Either a b] -> [a]
lefts' xs = 
