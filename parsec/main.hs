import Text.ParserCombinators.Parsec
import Program

parseOWLS input = parse parseSubprogram "" input

test = parseOWLS "main () {} oi"