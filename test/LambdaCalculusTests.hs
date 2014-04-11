module LambdaCalculusTests(
	allLambdaCalculusTests) where

import LambdaCalculus
import Test.HUnit

allLambdaCalculusTests = runTestTT tests

tests = TestList
	[toSKI_var
	,toSKI_int
	,toSKI_bool
	,toSKI_float
	,toSKI_char
	,toSKI_constAp
	,toSKI_abs
	,toSKI_absK
	,toSKI_absAp]

toSKI_var = toSKITest (var "x") (var "x")

toSKI_int = toSKITest (int 12) (int 12)

toSKI_bool = toSKITest (bool True) (bool True)

toSKI_float = toSKITest (float 12.3) (float 12.3)

toSKI_char = toSKITest (char 'a') (char 'a')

toSKI_constAp = toSKITest
	(ap (char 'b') (bool False))
	(ap (char 'b') (bool False))

toSKI_abs = toSKITest
	(ab (var "x") (var "x")) i

toSKI_absK = toSKITest
	(ab (var "x") (int 5)) (ap k (int 5))

toSKI_absAp = toSKITest
	(ab (var "var")
		(ap (ap (var "+") (var "var")) (int 4)))
	(ap (ap s (ap (ap s (ap k (var "+"))) i))
		(ap k (int 4)))

toSKITest input expected = TestCase
	(assertEqual ("Input: " ++ show input)
		expected
		(toSKI input))