module Replace where

    replaceWithP :: a -> Char
    replaceWithP = const 'p'

    lms :: [Maybe [Char]]
    lms = [Just "ave", Nothing, Just "woohoo"]

    liftedReplace :: [Maybe [Char]] -> [Char] 
    liftedReplace = fmap replaceWithP

    twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
    twiceLifted' = (fmap . fmap) replaceWithP

    thriceLifted :: [Maybe [Char]] -> [Maybe [Char]]
    thriceLifted = (fmap . fmap . fmap) replaceWithP

    main :: IO ()
    main = do
        print (replaceWithP lms)

        print (liftedReplace lms)

        print (twiceLifted' lms)

        print(thriceLifted lms)