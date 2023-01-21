module LearnParsers where 

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- char :: Char -> Parser Char
one = char '1'

one' = one >> stop

-- type Parser a = String -> Maybe (a, String)
-- Take a string value and produce a result a
-- or fail -> Nothing
-- return a tuple of the result and unconsumed String

