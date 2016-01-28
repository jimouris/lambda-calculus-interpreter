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

-- Computes Normal Form --
reduce :: Term -> Term
reduce x = x

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
main :: IO ()
main = do  
    putStrLn "Type a lambda expression like \"(\\\\x.\\\\y.x)\":"  
    inputStr <- readLn 
    let parsedString = myparse inputStr  
    putStrLn ("Normal form of " ++ inputStr ++ ": " ++ prettyprint parsedString)  


---------------------------------------- TEST CASES ---------------------------------------
inputString = "(\\x.\\y.x)(\\z.z)"
parseInputString = myparse inputString
myterm = Application (Abstraction "x" ( Abstraction "y"  (Var "x"))) (Abstraction "z" ( Var "z"))
prettyPrinted = prettyprint myterm





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
--lexp = \m n -> n m

{- Booleans -}
ltrue = \x y -> x
lfalse = \x y -> y
