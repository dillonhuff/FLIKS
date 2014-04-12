module LexerTests(
	allLexerTests) where

import ErrorHandling
import Lexer
import Test.HUnit

allLexerTests = runTestTT tests

tests = TestList
	[lexer_Number
	,lexer_Id
	,lexer_True
	,lexer_False
	,lexer_case
	,lexer_of
	,lexer_data
	,lexer_def
	,lexer_let
	,lexer_equal
	,lexer_in
	,lexer_if
	,lexer_then
	,lexer_else
	,lexer_nil
	,lexer_bar
	,lexer_arrow
	,lexer_minusGT
	,lexer_lparen
	,lexer_rparen
	,lexer_lambda
	,lexer_dot
	,lexer_overlapResAndId
	,lexer_plus
	,lexer_minus
	,lexer_times
	,lexer_divide
	,lexer_eqeq
	,lexer_lte
	,lexer_gte
	,lexer_lt
	,lexer_gt
	,lexer_not
	,lexer_and
	,lexer_or
	,lexer_orBar
	,lexer_char
	,lexer_floatNoExp
	,lexer_leadZeros
	,lexer_expFloat
	,lexer_negExpFloat
	,lexer_floatENodDecimal]

lexer_Number =
	tokenTest "1239" [(IntTok 1239)]

lexer_Id =
	tokenTest "a" [(Var "a")]

lexer_True =
	tokenTest "True" [(BoolTok True)]

lexer_False =
	tokenTest "False" [(BoolTok False)]

lexer_case =
	tokenTest "case" [CASE]

lexer_of =
	tokenTest "of" [OF]

lexer_let =
	tokenTest "let" [LET]

lexer_equal =
	tokenTest "=" [EQUAL]

lexer_in =
	tokenTest "in" [IN]

lexer_if =
	tokenTest "if" [IF]

lexer_then =
	tokenTest "then" [THEN]

lexer_else =
	tokenTest "else" [ELSE]

lexer_bar =
	tokenTest "|" [BAR]

lexer_arrow =
	tokenTest "->" [ARROW]

lexer_minusGT =
	tokenTest "- >" [(Var "-"), (Var ">")]

lexer_nil =
	tokenTest "nil" [NIL]

lexer_lparen =
	tokenTest "(" [LPAREN]

lexer_rparen =
	tokenTest ")" [RPAREN]

lexer_lambda =
	tokenTest "\\" [LAMBDA]

lexer_data =
	tokenTest "data" [DATA]

lexer_dot =
	tokenTest "." [DOT]

lexer_def =
	tokenTest "def" [DEF]

lexer_overlapResAndId =
	tokenTest "letter" [(Var "letter")]

lexer_plus =
	tokenTest "+" [(Var "+")]

lexer_minus =
	tokenTest "-" [(Var "-")]

lexer_times =
	tokenTest "*" [(Var "*")]

lexer_divide =
	tokenTest "/" [(Var "/")]

lexer_eqeq =
	tokenTest "==" [(Var "==")]

lexer_lte =
	tokenTest "<=" [(Var "<=")]

lexer_gte =
	tokenTest ">=" [(Var ">=")]

lexer_lt =
	tokenTest "<" [(Var "<")]

lexer_gt =
	tokenTest ">" [(Var ">")]

lexer_not =
	tokenTest "~" [(Var "~")]

lexer_and =
	tokenTest "&&" [(Var "&&")]

lexer_or =
	tokenTest "||" [(Var "||")]

lexer_orBar =
	tokenTest "|| |" [(Var "||"), BAR]

lexer_char =
	tokenTest "'c'" [(CharTok 'c')]

lexer_floatNoExp =
	tokenTest "3.120" [(FloatTok 3.120)]

lexer_leadZeros =
	tokenTest "000000.00312" [(FloatTok 0.00312)]

lexer_expFloat =
	tokenTest "12.223e23" [(FloatTok 12.223e23)]

lexer_negExpFloat =
	tokenTest "000.0125e-2" [(FloatTok 0.000125)]

lexer_floatENodDecimal =
	tokenTest "233e2" [(FloatTok 233e2)]

tokenTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(map tok $ extractValue $ lexer input))