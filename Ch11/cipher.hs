module Cipher where

  import Data.Char

  -- Vigenère cipher (only capitals)
  applyRot :: Int -> Int
  applyRot x = if x <= ord 'z' then x else ord 'a' + (x `mod` (ord 'a')) `mod` 26

  shiftVal :: Char -> Int
  shiftVal x = ord x - ord 'a'

  vigenere :: String -> String -> String
  vigenere str key = map vigChar (zipWithSpaces str (cycle key))

  zipWithSpaces :: [Char] -> [Char] -> [(Char, Char)]
  zipWithSpaces [] _ = []
  zipWithSpaces _ [] = []
  zipWithSpaces (a:as) (b:bs) =
    if a == ' '
      then (' ', ' ') : zipWithSpaces as (b:bs)
      else (a, b) : zipWithSpaces as bs

  vigChar :: (Char, Char) -> Char
  vigChar (' ', ' ') = ' '
  vigChar (c, keyChar) = chr (applyRot (ord c + shiftVal keyChar))
