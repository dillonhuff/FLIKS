module LambdaCalculus(
	Term, var, int, bool, float, char, ap, ab, i, k, s,
	makeIf, makeLet,
	toSKI) where

data Term
	= Var String
	| Integer Int
	| Boolean Bool
	| Floating Float
	| Character Char
	| Ap Term Term
	| Abs Term Term
	deriving (Eq)

instance Show Term where
	show = showTerm

showTerm :: Term -> String
showTerm (Var name) = name
showTerm (Integer n) = show n
showTerm (Boolean b) = show b
showTerm (Floating f) = show f
showTerm (Character c) = show c
showTerm (Ap t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
showTerm (Abs var t) = "(\\" ++ show var ++ " . " ++ show t ++ ")"

-- Constructors for Terms
var :: String -> Term
var name = Var name

int :: Int -> Term
int n = Integer n

bool :: Bool -> Term
bool b = Boolean b

float :: Float -> Term
float f = Floating f

char :: Char -> Term
char c = Character c

ap :: Term -> Term -> Term
ap t1 t2 = Ap t1 t2

ab :: Term -> Term -> Term
ab t1 t2 = Abs t1 t2

i :: Term
i = Var "I"

k :: Term
k = Var "K"

s :: Term
s = Var "S"

makeIf :: Term -> Term -> Term -> Term
makeIf cond e1 e2 = ap (ap (ap (var "if") cond) e1) e2

makeLet :: Term -> Term -> Term -> Term
makeLet var sub e = ap (ab var e) sub

toSKI :: Term -> Term
toSKI (Abs var t) = abConv var (toSKI t)
toSKI t = t

abConv :: Term -> Term -> Term
abConv v (Ap t1 t2) = ap (ap s (abConv v t1)) (abConv v t2)
abConv v t = if v == t
	then i
	else (ap k t)