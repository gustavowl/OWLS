import Text.ParserCombinators.Parsec
import System.IO
import Program

main :: IO() 
main = do
  s <- getContents
  print (parseOWLS s)
