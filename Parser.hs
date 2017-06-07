module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Data.Functor.Identity
import Control.Monad
import ProgramTree
import Expr
import TypeExpr
import Lexer
import qualified Tokens as T

---------------------------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------------------------

parseOWLS :: String -> Either ParseError Program
parseOWLS input = runParser parseProgram () "" (T.getTokens input)

parseProgram :: OWLParser Program
parseProgram = do
	dec <- many parseDeclaration
	main <- parseMain
	return (dec, main)

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------

parseDeclaration :: OWLParser Declaration
parseDeclaration = (try parseVarDec) <|> (try parseFuncDec) <|> parseProcDec

parseMain :: OWLParser Declaration
parseMain = do
	mainToken
	params <- parseParamList
	body <- parseBlock <|> parseEmptyBlock
	return $ Function "main" params (AtomicType "int") body

parseVarDec :: OWLParser Declaration
parseVarDec = do
	name <- identifier
	colon
	t <- parseVarType
	v <- optionMaybe parseInitialValue
	semi
	return $ Var name t v

parseFuncDec :: OWLParser Declaration
parseFuncDec = do
	funcToken
	name <- identifier
	params <- parseParamList
	colon
	ret <- parseVarType
	body <- parseBlock <|> parseEmptyBlock
	return $ Function name params ret body

parseEmptyBlock :: OWLParser [Statement]
parseEmptyBlock = do
	semi
	return []

parseProcDec :: OWLParser Declaration
parseProcDec = do
	procToken
	name <- identifier
	params <- parseParamList
	body <- parseBlock <|> parseEmptyBlock
	return $ Procedure name params body

parseParamList :: OWLParser [Declaration]
parseParamList = parens (sepBy parseParamDec comma)

parseParamDec :: OWLParser Declaration
parseParamDec = do
	name <- identifier
	colon
	t <- parseVarType <|> parseFuncType <|> parseProcType
	return $ Var name t Nothing

parseInitialValue :: OWLParser Expr
parseInitialValue = do
	assignToken
	expr <- parseExpr
	return expr

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

-- VarType : tipo esperado de retorno 
parseBlock :: OWLParser [Statement]
parseBlock = braces $ many parseStatement

parseStatement :: OWLParser Statement
parseStatement = (try parseProcRet)
	<|> (try parseFuncRet)
	<|> (try parseDecStatement) 
	<|> (try parseAssignment)
	<|> (try parseCondition)
	<|> (try parseProcCall)
	<|> (try parseWriteCall)
	-- TODO: other statement types

parseDecStatement :: OWLParser Statement
parseDecStatement = do
	parseDeclaration >>= f where
		f (Var n t v) = return $ VarDec (Var n t v)
		f (Function n p r b) = return $ FuncDec (Function n p r b)
		f (Procedure n p b) = return $ ProcDec (Procedure n p b)

parseProcRet :: OWLParser Statement
parseProcRet = do
	returnToken
	semi
	return ProcRet

parseFuncRet :: OWLParser Statement
parseFuncRet = do
	returnToken
	expr <- parseExpr
	semi
	return $ FuncRet expr

parseAssignment :: OWLParser Statement
parseAssignment = do
	name <- identifier
	assignToken
	expr <- parseExpr
	semi
	return $ Assignment name expr

parseCondition :: OWLParser Statement
parseCondition = do
	ifToken
	expr <- parseBoolExpr
	body <- parseBlock
	elseBody <- (try parseElse) <|> parseEmptyElse
	return $ If expr body elseBody

parseElse :: OWLParser [Statement]
parseElse = do
	elseToken
	body <- parseBlock
	return body

parseEmptyElse :: OWLParser [Statement]
parseEmptyElse = do
	return []

parseProcCall :: OWLParser Statement
parseProcCall = do
	name <- identifier
	args <- parens (sepBy parseExpr comma)
	semi
	return $ ProcCall name args

parseWriteCall :: OWLParser Statement
parseWriteCall = do
	writeToken
	arg <- parens parseExpr
	semi
	return $ WriteCall arg