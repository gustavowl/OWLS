import Text.ParserCombinators.Parsec
import qualified Parser
import qualified Interpreter

import System.Environment
import System.IO

main :: IO() 
main = do
  	args <- getArgs
  	fileText <- openFile (args !! 0) ReadMode
  	s <- hGetContents fileText
  	parseProgram s

parseProgram :: String -> IO()
parseProgram input = case 
	Parser.parseOWLS input of
		Left error -> print error
		Right program -> do
			--print program
			Interpreter.runProgram program