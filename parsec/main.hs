import Text.ParserCombinators.Parsec
import System.IO
import Program

parseOWLS input = parse parseProgram "" input

e = "x : int; \n" ++
	"func bla (a : int, b : int) : int {} \n" ++
	"proc ble (k : char) { } \n" ++
	"main() { \n" ++
	"\t x = 10; \n" ++
	"\t y = bla(10); \n" ++
	"\t z = y; \n" ++
--	"\t return \"\";\n" ++
	"}"

test = parseOWLS e
