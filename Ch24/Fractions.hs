module Text.Fractions where

import Control.Applicative 
import Data.Ratio ((%)) 
import Text.Trifecta

badFraction = "1/0" 
alsoBad = "10" 
shouldWork = "1/2" 
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

-- decimal :: Integral a => Parser a
-- char :: Char -> Parser Char