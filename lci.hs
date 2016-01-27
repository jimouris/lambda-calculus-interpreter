
{- Church Numerals -}
c0 = \f x -> x
c1 = \f x -> f x
c2 = \f x -> f (f x)

{- succ(n) = n + 1 -}
succ = \n f x -> f (n f x)

{- plus(m, n) = m + n -}
plus = \m n f x -> m f (n f x)
