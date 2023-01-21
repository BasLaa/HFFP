module Cipher where

  import Data.Char

  caesar :: Int -> String -> String
  caesar n str = map chr . map applyRot . map (+n) . map ord $ str

  uncaesar :: Int -> String -> String
  uncaesar n str = map chr . map revRot . map (subtract n) . map ord $ str

  applyRot :: Int -> Int
  applyRot x = if x <= ord 'z' then x else ord 'a' + (x `mod` (ord 'a')) `mod` 26

  revRot :: Int -> Int
  revRot x = if x >= ord 'a' then x else ord 'z' + 1 - (ord 'a' `mod` x) `mod` 26
