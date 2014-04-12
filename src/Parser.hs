 module Parser(
 	parseExpr) where

import ErrorHandling
import LambdaCalculus
import Lexer
import Text.Parsec
import Text.Parsec.Expr

parseExpr :: String -> Error Term
parseExpr programText = (lexer programText) >>= parseToks

parseToks :: [PosTok] -> Error Term
parseToks toks = case parse pExpr "Parser" toks of
		Left err -> Failed $ show err
		Right expr -> Succeeded expr

pParens toParse = do
	flikTok LPAREN
	v <- toParse
	flikTok RPAREN
	return v

pExpr = do
	exprs <- many1 pSExpr
	return $ multiExpr exprs

pSExpr = do
	expr <- pOpExpr
		<|> pAbsExpr
		<|> pIfExpr
		<|> pLetExpr
	return expr

pVar = do
	ident <- varTok
	return $ var $ varVal ident

pNum = do
	num <- pInt
		<|> pFloat
	return num

pInt = do
	numTok <- intTok
	return $ int $ intVal numTok

pFloat = do
	numTok <- floatTok
	return $ float $ floatVal numTok

pBool = do
	val <- boolTok
	return $ bool $ boolVal val

pChar = do
	val <- charTok
	return $ LambdaCalculus.char $ charVal val

pAbsExpr = do
	flikTok LAMBDA
	v <- pVar
	flikTok DOT
	expr <- pExpr
	return $ ab v expr

pIfExpr = do
	flikTok IF
	cond <- pExpr
	flikTok THEN
	e1 <- pExpr
	flikTok ELSE
	e2 <- pExpr
	return $ makeIf cond e1 e2

pLetExpr = do
	flikTok LET
	var <- pVar
	flikTok EQUAL
	sub <- pExpr
	flikTok IN
	e <- pExpr
	return $ makeLet var sub e

-- Parsing for expressions with builtin/infix operators
pOpExpr = buildExpressionParser table term

term = pParens pExpr
	<|> pBool
	<|> pChar
	<|> pInt
	<|> pFloat
	<|> pVar

table =
	[[Prefix doNegate, Prefix doLogicalNot]
	,[Infix doPlus AssocLeft, Infix doMinus AssocLeft]]

doNegate = do
	neg <- flikTok (Var "-")
	return $ negateTerm

doLogicalNot = do
	logNot <- flikTok (Var "~")
	return $ logicalNegateTerm

doPlus = do
	plusOp <- flikTok (Var "+")
	return $ plusTerms

doMinus = do
	plusOp <- flikTok (Var "-")
	return $ minusTerms

negateTerm :: Term -> Term
negateTerm t = ap (var "-") t

logicalNegateTerm :: Term -> Term
logicalNegateTerm t = ap (var "~") t

plusTerms :: Term -> Term -> Term
plusTerms t1 t2 = ap (ap (var "+") t1) t2

minusTerms :: Term -> Term -> Term
minusTerms t1 t2 = ap (ap (var "-") t1) t2

multiExpr :: [Term] -> Term
multiExpr [] = error "No expressions in input"
multiExpr [e] = e
multiExpr (e1:e2:rest) = foldl ap (ap e1 e2) rest

varTok :: (Monad m) => ParsecT [PosTok] u m PosTok
varTok = tokOfType (\t -> isVarTok t && (not $ isOperator t))

intTok :: (Monad m) => ParsecT [PosTok] u m PosTok
intTok = tokOfType isIntTok

floatTok :: (Monad m) => ParsecT [PosTok] u m PosTok
floatTok = tokOfType isFloatTok

boolTok :: (Monad m) => ParsecT [PosTok] u m PosTok
boolTok = tokOfType isBoolTok

charTok :: (Monad m) => ParsecT [PosTok] u m PosTok
charTok = tokOfType isCharTok

opTok :: (Monad m) => ParsecT [PosTok] u m PosTok
opTok = tokOfType isOperator

tokOfType :: (Monad m) => (Tok -> Bool) -> ParsecT [PosTok] u m PosTok
tokOfType isTokOfType = tokenPrim show updatePos idTok
	where
		idTok pt = if isTokOfType (tok pt) then Just pt else Nothing

flikTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
flikTok x = tokenPrim show updatePos testTok
	where
		testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos pos _ [] = pos