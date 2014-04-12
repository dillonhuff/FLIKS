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
	,parseExpr_integer
	,parseExpr_BoolExprTrue
	,parseExpr_BoolExprFalse
	,parseExpr_parenExpr
	,parseExpr_absExpr
	,parseExpr_apExpr
	,parseExpr_multiApExpr
	,parseExpr_ifExpr
	,parseExpr_letExpr]

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
	parseExprTest "let x = 34 in (\\y. (x + 2) y)"
			(ap (ab (var "x")
				(ab (var "y")
					(ap (ap (ap (var "+") (var "x")) (int 2)) (var "y"))))
						(int 34))

parseExprTest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(extractValue $ parseExpr input))