module ParserTests(
	allParserTests) where

import ErrorHandling
import LambdaCalculus
import Parser
import Test.HUnit

allParserTests = runTestTT tests

tests = TestList
	[parseExpr_var
	,parseExpr_negate
	,parseExpr_logNeg
	,parseExpr_plus
	,parseExpr_minus
	,parseExpr_doubleMinus
	,parseExpr_divide
	,parseExpr_multiply
	,parseExpr_integer
	,parseExpr_BoolExprTrue
	,parseExpr_BoolExprFalse
	,parseExpr_parenExpr
	,parseExpr_absExpr
	,parseExpr_gte
	,parseExpr_lte
	,parseExpr_gt
	,parseExpr_lt
	,parseExpr_eq
	,parseExpr_and
	,parseExpr_or
	,parseExpr_apExpr
	,parseExpr_multiApExpr
	,parseExpr_ifExpr
	,parseExpr_letExpr
	,parseProgram_tiny
	,parseProgram_oneArg
	,parseProgram_manyDefs]

parseExpr_var =
	parseExprTest "n12" (var "n12")

parseExpr_negate =
	parseExprTest "- 12" (ap (var "-") (int 12))

parseExpr_logNeg =
	parseExprTest "~True" (ap (var "~") (bool True))

parseExpr_plus =
	parseExprTest "1 + 2" (ap (ap (var "+") (int 1)) (int 2))

parseExpr_minus =
	parseExprTest "45 - 4.5" (ap (ap (var "-") (int 45)) (float 4.5))

parseExpr_doubleMinus =
	parseExprTest "45e2 - -1" (ap (ap (var "-") (float 45e2)) (ap (var "-") (int 1)))

parseExpr_divide =
	parseExprTest "23.2/1.9e3" (ap (ap (var "/") (float 23.2)) (float 1.9e3))

parseExpr_multiply =
	parseExprTest "652e2 * 2" (ap (ap (var "*") (float 652e2)) (int 2))

parseExpr_gte =
	parseExprTest "5 >= 9" (ap (ap (var ">=") (int 5)) (int 9))

parseExpr_lte =
	parseExprTest "5 <= 9" (ap (ap (var "<=") (int 5)) (int 9))

parseExpr_gt =
	parseExprTest "5 > 9" (ap (ap (var ">") (int 5)) (int 9))

parseExpr_lt =
	parseExprTest "5 < 9" (ap (ap (var "<") (int 5)) (int 9))

parseExpr_eq =
	parseExprTest "5 == 9" (ap (ap (var "==") (int 5)) (int 9))

parseExpr_and =
	parseExprTest "True && (1 + 2)"
		(ap
			(ap (var "&&") (bool True))
			(ap (ap (var "+") (int 1)) (int 2)))

parseExpr_or =
	parseExprTest "True || (1 - -2.23e38)"
		(ap
			(ap (var "||") (bool True))
			(ap (ap (var "-") (int 1)) (ap (var "-") (float 2.23e38))))

parseExpr_integer = parseExprTest "58263" (int 58263)

parseExpr_BoolExprTrue = parseExprTest "True" (bool True)

parseExpr_BoolExprFalse = parseExprTest "False" (bool False)

parseExpr_absExpr =
	parseExprTest "\\ x . (x 12)"
		(ab (var "x") (ap (var "x") (int 12)))

parseExpr_apExpr =
	parseExprTest "(12 True)"
		(ap (int 12) (bool True))

parseExpr_multiApExpr =
	parseExprTest "x 12 (13 False)"
		(ap
			(ap (var "x") (int 12))
			(ap (int 13) (bool False)))

parseExpr_parenExpr =
	parseExprTest "(n12)" (var "n12")

parseExpr_ifExpr =
	parseExprTest "if True then 12 else (- 1)"
		(makeIf (bool True)
			(int 12)
			(ap (var "-") (int 1)))

parseExpr_letExpr =
	parseExprTest "let x = -34 in (\\y. (x + 2) y)"
			(ap (ab (var "x")
				(ab (var "y")
					(ap (ap (ap (var "+") (var "x")) (int 2)) (var "y"))))
						(ap (var "-") (int 34)))

parseExprTest input expected = functionTest parseExpr input expected

parseProgram_tiny =
	parseProgramTest "def prog = 12"
		[(var "prog", int 12)]

parseProgram_oneArg =
	parseProgramTest "def prog x = 45.5"
		[(var "prog", ab (var "x") (float 45.5))]

parseProgram_manyDefs =
	parseProgramTest
		"def square x = x * x def no a b c = if a then b else c def prog = no True (square 1) (square 2)"
		[(var "square", ab (var "x") (ap (ap (var "*") (var "x")) (var "x")))
		,(var "no",
			(ab (var "c")
			(ab (var "b")
			(ab (var "a") (ap (ap (ap (var "if") (var "a")) (var "b")) (var "c"))))))
		,(var "prog", ap (ap (ap (var "no") (bool True)) (ap (var "square") (int 1))) (ap (var "square") (int 2)))]

parseProgramTest input expected = functionTest parseProgram input expected

functionTest func input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(extractValue $ func input))