------------------------------- Lambda Calculus Interpreter -------------------------------

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import System.IO
import Data.List
import Data.Char
import Text.Parsec
import Text.Parsec.String
import qualified Text.PrettyPrint as PP

data Term = Var String
          | Application Term Term
          | Abstraction String Term
          deriving(Show, Eq)

data Result = Res Term Int [Term] [String] deriving(Show, Eq)


delete1 x [] = []
delete1 x (y:xs) | (x == y) = (delete1 x xs)
                | otherwise = (y:delete1 x xs)

freeVars :: Term -> [String]
freeVars t = case t of  Var s -> [s]
                        Application t1 t2 -> freeVars t1 ++ freeVars t2
                        Abstraction s1 t2 -> delete1 s1 (freeVars t2)

boundVars :: Term -> [String]
boundVars (Var v) = []
boundVars (Application t1 t2) = boundVars t1 ++ boundVars t2
boundVars (Abstraction s1 t2) = [s1] ++ boundVars t2

--renameVarinTerm :: Term -> String -> String -> Term
renameVarinTerm (Var s1) s s' = if (s1 == s) then (Var s') else (Var s1)
renameVarinTerm (Abstraction s1 t) s s' = if (s1 /= s) then (Abstraction s1 (renameVarinTerm t s s')) else (Abstraction s1 t)
renameVarinTerm (Application t1 t2) s s' = (Application (renameVarinTerm t1 s s') (renameVarinTerm t2 s s'))

------- Alpha reduce applies only to abstraction! ------
azList :: [String]
azList = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

alphaReduction :: String->Term -> Term
alphaReduction str (Abstraction s t) = (Abstraction s' t') where
    s' = str
    t' = renameVarinTerm t s s'
alphaReduction str term = term

alphaReduceAll :: [String]->Term -> Term
alphaReduceAll (s:strs) (Application t1 t2) = (Application t1 t2)
alphaReduceAll (s:strs) (Abstraction s1 t1) = (alphaReduction s (Abstraction s1 (alphaReduceAll strs t1)))
alphaReduceAll (s:strs) (Var s1) = (Var s1)

replace :: Term->String->Term -> Term
replace (Var s1) str trepl = if (str == s1) then trepl else (Var s1)
replace (Abstraction s1 t1) str trepl = if (str == s1) then (replace t1 str trepl) else (Abstraction s1 (replace t1 str trepl))
replace (Application t1 t2) str trepl = (Application (replace t1 str trepl) (replace t2 str trepl))

betaReduction :: Term -> Term
betaReduction term = case term of
    (Var s) -> (Var s)
    (Application (Var v1) t) -> (Application (Var v1) t)
    (Application (Abstraction abstr t) appt) -> (replace (Abstraction abstr t) abstr appt)
    (Application (Application t1 t2) appt) -> (Application (betaReduction (Application t1 t2)) appt)
    (Abstraction s t) -> (Abstraction s t)

--reduce :: Term -> Term
--reduce (Var s) = (Var s)
--reduce (Application (Var v1) t) = (Application (Var v1) (reduce t))
--reduce (Abstraction str1 t1) = case t1 of
--    (Application t2 (Var str2)) -> if (str1 == str2 && (notElem str1 (freeVars t2))) then (reduce t2) else (Abstraction str1 (reduce t1)) --eta Reduction
--    otherwise -> (Abstraction str1 (reduce t1))
--reduce (Application t1 t2) = reduce (betaReduction (Application t1' t2))
--    where
--        t1' = (alphaReduceAll intrsct t1)
--            where
--                intrsct = azList \\ varsInT
--                varsInT = (freeVars t2) ++ (boundVars t2)
----reduce (Application (Application t1 t2) t3) = (Application (reduce (Application t1 t2)) t3)

reduceOne :: Term -> Term
reduceOne (Var s) = (Var s)
reduceOne (Application (Var v1) t) = (Application (Var v1) (reduceOne t))
reduceOne (Application (Application t1 t2) t3) = (Application (reduceOne (Application t1 t2)) t3)
reduceOne (Abstraction str1 t1) = case t1 of
    (Application t2 (Var str2)) -> if (str1 == str2 && (notElem str1 (freeVars t2))) then t2 else (Abstraction str1 (reduceOne t1)) --eta Reduction
    otherwise -> (Abstraction str1 (reduceOne t1))
reduceOne (Application t1 t2) = (betaReduction (Application t1' t2))
    where
        t1' = (alphaReduceAll intrsct t1)
            where
                intrsct = azList \\ varsInT
                varsInT = (freeVars t2) ++ (boundVars t2)

myResult :: Term -> [String]
myResult t1 = if (t1 == reduceOne t1) then [prettyprint t1] else [prettyprint t1]++(myResult t2) where 
    t2 = reduceOne t1

--------------------------------------- PARSER --------------------------------------------
lambdaTerm :: Parser Term
lambdaTerm = lambdaAbstraction <|> lambdaApplication <|> simple

lambdaAbstraction :: Parser Term
lambdaAbstraction = do
    char '\\'
    var <- letter
    char '.'
    body <- lambdaTerm
    return(Abstraction [var] body)

lambdaApplication :: Parser Term
lambdaApplication = do
    apps <- many1 simple
    return(foldl1 Application apps)

simple :: Parser Term
simple = lambdaVar <|> paren

lambdaVar :: Parser Term
lambdaVar = do
    var <- letter
    return(Var [var])

paren :: Parser Term
paren = do
    char '('
    term <- lambdaTerm
    char ')'
    return term

myparse :: String -> Term
myparse str = case (parse lambdaTerm "" str) of
    Left msg -> error $ show msg
    Right term' -> term'

test = myparse "\\z.(\\f.\\x.fzx)(\\y.y)"
pair = myparse "\\x.\\y.\\z.zxy"


-------------------------------------- PRETTY PRINT --------------------------------------
ppr :: Term -> PP.Doc
ppr (Var x) = PP.text x
ppr (Abstraction x e) = PP.fcat [(PP.fcat [PP.text "\\",PP.text x,PP.text "."]),(ppr e)]
ppr apply = PP.fcat (map parenApp (args apply))

args (Application x y) = args x ++ [y]
args x = [x]

parenApp (x@(Application _ _)) = PP.parens (ppr x)
parenApp (x@(Abstraction _ _)) = PP.parens (ppr x)
parenApp x = ppr x

prettyprint :: Term -> String
prettyprint term = PP.render (ppr term)


------------------------------------------- Main ------------------------------------------
loopPrinter = do 
    putStr "> "
    inputStr <- readLn 
    let parsedString = myparse inputStr  
    putStrLn ("Normal form of " ++ inputStr ++ ": " ++ prettyprint parsedString)
    loopPrinter

main :: IO ()
main = do  
    putStrLn ("Type a lambda expression like \"(\\\\x.\\\\y.x)\" or ^D to exit:")
    loopPrinter

---------------------------------------- TEST CASES ---------------------------------------
inputString = "(\\x.\\y.x)(\\z.z)"
parseInputString = myparse inputString
myterm = Application (Abstraction "x" ( Abstraction "y"  (Var "x"))) (Abstraction "z" ( Var "z"))
prettyPrinted = prettyprint myterm

--betaReduction (myparse "(\\x.x)(\\y.y)")
--betaReduction (myparse "((\\x.x)(\\y.y))(\\z.z)")

------------------------------------- Church Numerals -------------------------------------
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
lexp = (\m -> (\n -> n m))

{- Booleans -}
ltrue = (\x -> (\y -> x))
lfalse = (\x -> (\y -> y))

iszero = (\n -> ((n (\x -> lfalse)) ltrue))
