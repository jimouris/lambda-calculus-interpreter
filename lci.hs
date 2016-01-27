{- Lambda Calculus Interpreter -}

{- Church Numerals -}
type Church a = (a -> a) -> a -> a

church :: Integer -> Church Integer
church 0 = \f -> \x -> x
church n = \f -> \x -> f (church (n-1) f x)

unchurch :: Church Integer -> Integer
unchurch cn = cn (+ 1) 0

--c0 = \f x -> x
--c1 = \f x -> f x
--c2 = \f x -> f (f x)

{- lsucc(n) = n + 1 -}
lsucc = \n f x -> f (n f x)

{- lplus(m, n) = m + n -}
lplus = \m n f x -> m f (n f x)

{- lmult(m, n) = m * n -}
lmult = \m n f -> m (n f)

{- lexp(m, n) = m ^ n -}
lexp = \m n -> n m


{- Main -}
main :: IO ()
main = do let z = unchurch (lsucc (church 4))
          putStrLn $ "The result is: " ++ show z
