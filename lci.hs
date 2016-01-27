{- Lambda Calculus Interpreter -}

{- Church Numerals -}
type Church a = (a -> a) -> a -> a

church :: Integer -> Church Integer
church 0 = \f -> \x -> x
church n = \f -> \x -> f (church (n-1) f x)

unchurch :: Church Integer -> Integer
unchurch cn = cn (+ 1) 0

{- lsucc n = n + 1 -}
lsucc :: Church Integer -> Church Integer
lsucc = \n f x -> f (n f x)

{- lplus m n = m + n -}
lplus :: Church Integer -> Church Integer -> Church Integer
lplus = \m n f x -> m f (n f x)

{- lmult m n = m * n -}
lmult :: Church Integer -> Church Integer -> Church Integer
lmult = \m n f -> m (n f)

{- lexp m n = m ^ n -}
--lexp :: Church Integer -> Church Integer -> Church Integer
lexp = \m n -> n m

{- Booleans -}
ltrue = \x y -> x
lfalse = \x y -> y

{- Main -}
main :: IO ()
main = do let z = unchurch (lmult (church 4) (church 2))
          putStrLn $ "The result is: " ++ show z
