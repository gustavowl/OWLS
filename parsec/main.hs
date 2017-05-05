import Text.ParserCombinators.Parsec
import System.IO
import Program

parseOWLS input = parse parseProgram "" input

e = "x : int; \n" ++
	"func bla (a : int, b : int) : int {}" ++
	"proc ble (k : char) { }" ++
	"main() { \n" ++
	"\t x = 10; \n" ++
--	"\t return \"\";\n" ++
	"}"

test = parseOWLS e
