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
	"\t w = bla(10, 5); \n" ++
	"\t n = x * 10 + y - z * -w; \n" ++ 
	"\t p = true; \n" ++
	"\t q = p or false; \n" ++
	"\t bla = y < x and p or y * z == n + 2; \n" ++
--	"\t return \"\";\n" ++
	"}"

test = parseOWLS e
