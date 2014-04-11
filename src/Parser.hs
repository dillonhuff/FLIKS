 module Parser(
 	parseExpr) where

import ErrorHandling
import LambdaCalculus
import Lexer
import Text.Parsec

parseExpr :: String -> Error Term
parseExpr programText = (lexer programText) >>= parseToks

parseToks :: [PosTok] -> Error Term
parseToks toks = case parse pExpr "Parser" toks of
		Left err -> Failed $ show err
		Right expr -> Succeeded expr

pParens toParse = do
	tflTok LPAREN
	v <- toParse
	tflTok RPAREN
	return v

pExpr = do
	exprs <- many1 pSExpr
	return $ multiExpr exprs

pApExpr = do
	sExpr <- pSExpr
	expr <- pExpr
	return $ ap sExpr expr

pSExpr = do
	expr <- pParens pExpr
		<|> pInfix
		<|> pVarExpr
		<|> pNumExpr
		<|> pBoolExpr
		<|> pAbsExpr
		<|> pIfExpr
		<|> pLetExpr
	return expr

pInfix = do
	startE <- pSExpr
	op <- pInfixOp
	endE <- pSExpr
	return $ ap (ap op startE) endE

pInfixOp = do
	op <- infixOp
	return $ var $ varVal op

pVarExpr = do
	ident <- varTok
	return $ var $ varVal ident

pNumExpr = do
	num <- pIntTok
		<|> pFloatTok
	return num

pIntTok = do
	numTok <- intTok
	return $ int $ intVal numTok

pFloatTok = do
	numTok <- floatTok
	return $ float $ floatVal numTok

pBoolExpr = do
	val <- boolTok
	return $ bool $ boolVal val

pAbsExpr = do
	tflTok LAMBDA
	v <- pVarExpr
	tflTok DOT
	expr <- pExpr
	return $ ab v expr

pIfExpr = do
	tflTok IF
	cond <- pExpr
	tflTok THEN
	e1 <- pExpr
	tflTok ELSE
	e2 <- pExpr
	return $ makeIf cond e1 e2

pLetExpr = do
	tflTok LET
	var <- pVarExpr
	tflTok EQUAL
	sub <- pExpr
	tflTok IN
	e <- pExpr
	return $ makeLet var sub e

multiExpr :: [Term] -> Term
multiExpr [] = error "No expressions in input"
multiExpr [e] = e
multiExpr (e1:e2:rest) = foldl ap (ap e1 e2) rest

varTok :: (Monad m) => ParsecT [PosTok] u m PosTok
varTok = tokOfType isVarTok

intTok :: (Monad m) => ParsecT [PosTok] u m PosTok
intTok = tokOfType isIntTok

floatTok :: (Monad m) => ParsecT [PosTok] u m PosTok
floatTok = tokOfType isFloatTok

boolTok :: (Monad m) => ParsecT [PosTok] u m PosTok
boolTok = tokOfType isBoolTok

charTok :: (Monad m) => ParsecT [PosTok] u m PosTok
charTok = tokOfType isCharTok

infixOp :: (Monad m) => ParsecT [PosTok] u m PosTok
infixOp = tokOfType infixOperator

tokOfType :: (Monad m) => (Tok -> Bool) -> ParsecT [PosTok] u m PosTok
tokOfType isTokOfType = tokenPrim show updatePos idTok
	where
		idTok pt = if isTokOfType (tok pt) then Just pt else Nothing

tflTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
tflTok x = tokenPrim show updatePos testTok
	where
		testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos pos _ [] = pos