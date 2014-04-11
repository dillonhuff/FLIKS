module Lexer(
	PosTok(PT), tok, pos, dummyPosTok,
	num, bool,
	lexer,
	Tok(Var, IntTok, BoolTok, DEF, LET, EQUAL,
		IN, IF, THEN, ELSE, NIL, LPAREN,
		RPAREN, LAMBDA, DOT),
	isIntTok, isVarTok, isBoolTok) where

import ErrorHandling
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec

data PosTok = PT Tok SourcePos
	
instance Show PosTok where
	show = showPT

showPT :: PosTok -> String
showPT (PT t _) = show t

tok :: PosTok -> Tok
tok (PT t _) = t

pos :: PosTok -> SourcePos
pos (PT _ p) = p

dummyPosTok :: Tok -> PosTok
dummyPosTok t = PT t (newPos "DUMMY" 0 0)

instance Eq PosTok where
	(==) = ptEq

ptEq (PT t1 _) (PT t2 _) = t1 == t2

data Tok
	= Var String
	| IntTok Int
	| BoolTok Bool
	| DEF
	| LET
	| EQUAL
	| IN
	| IF
	| THEN
	| ELSE
	| NIL
	| LPAREN
	| RPAREN
	| LAMBDA
	| DOT
	deriving (Eq, Show)

num (IntTok n) = n
num t = error $ show t ++ " is not a number"

bool (BoolTok b) = b
bool t = error $ show t ++ " is not a boolean"

isVarTok (Var _) = True
isVarTok _ = False

isIntTok (IntTok _) = True
isIntTok _ = False

isBoolTok (BoolTok _) = True
isBoolTok _ = False

resToTok =
	[("let", LET), ("=", EQUAL), ("def", DEF), ("in", IN), ("if", IF)
	,("then", THEN), ("else", ELSE), ("nil", NIL), ("(", LPAREN)
	,(")", RPAREN), ("\\", LAMBDA), (".", DOT)]

lexer :: String -> Error [PosTok]
lexer str = case parse pToks "TFL" str of
	Left err -> Failed $ show err
	Right toks -> Succeeded toks

pToks = do
	spaces
	ts <- endBy pTok spaces
	return ts

pTok = do
	t <- pIntTok
		<|> pVarOrRes
		<|> pBool
	return t

pIntTok = do
	pos <- getPosition
	digs <- many1 digit
	return $ PT (IntTok $ read digs) pos

pVarOrRes = do
	pos <- getPosition
	idOrRes <- pId
		<|> try pOp
		<|> pResName
	return $ case lookup idOrRes resToTok of
		Just t -> PT t pos
		Nothing -> PT (Var idOrRes) pos

pId = do
	startChar <- lower
	rest <- many alphaNum
	return (startChar:rest)

pOp = do
	op <- string "+"
		<|> string "-"
		<|> string "*"
		<|> string "/"
		<|> string "=="
		<|> try (string "<=")
		<|> try (string ">=")
		<|> string "<"
		<|> string ">"
		<|> string "~"
		<|> string "&&"
		<|> string "||"
	return op

pResName = do
	rName <- string "="
		<|> string "\\"
		<|> string "."
		<|> string "("
		<|> string ")"
		<|> string ";"
	return rName

pBool = do
	b <- pTrue
		<|> pFalse
	return b

pTrue = do
	pos <- getPosition
	val <- string "True"
	return $ PT (BoolTok True) pos

pFalse = do
	pos <- getPosition
	val <- string "False"
	return $ PT (BoolTok False) pos
