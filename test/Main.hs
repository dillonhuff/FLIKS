module Main(main) where

import LambdaCalculusTests
import LexerTests
import ParserTests

main = do
	allLambdaCalculusTests
	allLexerTests
	allParserTests

