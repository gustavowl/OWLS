import Text.ParserCombinators.Parsec
import System.IO
import qualified Parser
import qualified Interpreter

main :: IO() 
main = do
  s <- getContents
  parseProgram s

parseProgram :: String -> IO()
parseProgram input = case 
	Parser.parseOWLS input of
		Left error -> print error
		Right program -> Interpreter.runProgram program