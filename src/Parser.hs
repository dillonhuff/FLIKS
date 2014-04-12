 module Parser(
 	parseProgram,
 	parseExpr) where

import ErrorHandling
import LambdaCalculus
import Lexer
import Text.Parsec
import Text.Parsec.Expr

-- A program is a list of terms synonyms with distinguished
-- synonym prog
type Program = [(Term, Term)]

parseProgram :: String -> Error Program
parseProgram program = (lexer program) >>= parseProgToks

parseProgToks :: [PosTok] -> Error Program
parseProgToks toks = case parse pProgram "Parser" toks of
	Left err -> Failed $ show err
	Right prog -> case lookup (var "prog") prog of
		Just def -> Succeeded prog
		Nothing -> Failed $ "Error: Program has no function named prog"

parseExpr :: String -> Error Term
parseExpr exprText = (lexer exprText) >>= parseToks

parseToks :: [PosTok] -> Error Term
parseToks toks = case parse pExpr "Parser" toks of
		Left err -> Failed $ show err
		Right expr -> Succeeded expr

createDef :: [Term] -> Term -> (Term, Term)
createDef header body = (head header, wrapLambdas (reverse $ tail header) body)

wrapLambdas :: [Term] -> Term -> Term
wrapLambdas [] t = t
wrapLambdas (v:vs) t = wrapLambdas vs (ab v t)

pProgram = do
	exprDefs <- many1 pExprDef
	return $ exprDefs

pExprDef = do
	fliksTok DEF
	header <- many1 pVar
	fliksTok EQUAL
	body <- pExpr
	return $ createDef header body

pParens toParse = do
	fliksTok LPAREN
	v <- toParse
	fliksTok RPAREN
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
	fliksTok LAMBDA
	v <- pVar
	fliksTok DOT
	expr <- pExpr
	return $ ab v expr

pIfExpr = do
	fliksTok IF
	cond <- pExpr
	fliksTok THEN
	e1 <- pExpr
	fliksTok ELSE
	e2 <- pExpr
	return $ makeIf cond e1 e2

pLetExpr = do
	fliksTok LET
	var <- pVar
	fliksTok EQUAL
	sub <- pExpr
	fliksTok IN
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
	[[prefixOp "-", prefixOp "~"]
	,[infixOp "*", infixOp "/"]
	,[infixOp "+", infixOp "-"]
	,[infixOp ">", infixOp "<", infixOp ">=", infixOp "<="]
	,[infixOp "=="]
	,[infixOp "&&"]
	,[infixOp "||"]]

prefixOp opStr = Prefix $ doUnoperator opStr
infixOp opStr = Infix (doBinop opStr) AssocLeft

doUnoperator opStr = do
	opVal <- fliksTok (Var opStr)
	return $ unop (var opStr)

doBinop opStr = do
	opVal <- fliksTok (Var opStr)
	return $ binop (var opStr)

unop :: Term -> Term -> Term
unop un t = ap un t

binop :: Term -> Term -> Term -> Term
binop bop t1 t2 = ap (ap (bop) t1) t2

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

fliksTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
fliksTok x = tokenPrim show updatePos testTok
	where
		testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos pos _ [] = pos