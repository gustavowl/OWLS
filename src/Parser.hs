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
	types <- many parseUserType
	decs <- many parseDeclaration
	main <- parseMain
	return (types, decs, main)

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
	t <- parseVarType
	return $ Var name t Nothing

parseInitialValue :: OWLParser Expr
parseInitialValue = do
	assignToken
	expr <- parseExpr
	return expr

parseUserType :: OWLParser UserType
parseUserType = do
	structToken
	name <- identifier
	fields <- braces (many parseVarDec)
	return (name, fields)

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

-- VarType : tipo esperado de retorno 
parseBlock :: OWLParser [Statement]
parseBlock = braces $ many parseStatement

parseStatement :: OWLParser Statement
parseStatement = (try parseDecStatement) 
	<|> (try $ parseSemi parseProcCall)
	<|> (try $ parseSemi parseWriteCall)
	<|> (try $ parseSemi parseProcRet)
	<|> (try $ parseSemi parseFuncRet)
	<|> (try $ parseSemi parseAssignment)
	<|> (try parseCondition)
	<|> (try parseWhile)
	<|> (try parseFor)
	-- TODO: other statement types

parseSemi :: OWLParser Statement -> OWLParser Statement
parseSemi parser = do
	s <- try parser
	semi
	return s

parseDecStatement :: OWLParser Statement
parseDecStatement = do
	parseDeclaration >>= f where
		f (Var n t v) = return $ VarDec (Var n t v)
		f (Function n p r b) = return $ FuncDec (Function n p r b)
		f (Procedure n p b) = return $ ProcDec (Procedure n p b)

parseProcCall :: OWLParser Statement
parseProcCall = do
	name <- identifier
	args <- parens (sepBy parseExpr comma)
	return $ ProcCall name args

parseWriteCall :: OWLParser Statement
parseWriteCall = do
	writeToken
	arg <- parens parseExpr
	return $ WriteCall arg

parseProcRet :: OWLParser Statement
parseProcRet = do
	returnToken
	return ProcRet

parseFuncRet :: OWLParser Statement
parseFuncRet = do
	returnToken
	expr <- parseExpr
	return $ FuncRet expr

parseAssignment :: OWLParser Statement
parseAssignment = do
	name <- identifier
	assignToken
	expr <- parseExpr
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

parseWhile :: OWLParser Statement
parseWhile = do
	whileToken
	expr <- parens parseBoolExpr
	body <- parseBlock
	return $ While expr body

parseFor :: OWLParser Statement
parseFor = do
	forToken
	lparen
	ini <- parseForInit
	semi
	expr <- parseBoolExpr
	semi
	incr <- parseAssignment
	rparen
	body <- parseBlock
	return $ For ini expr incr body

parseForInit :: OWLParser Declaration
parseForInit = do
	name <- identifier
	colon
	t <- parseVarType
	v <- parseInitialValue
	return $ Var name t $ Just v

parseBreak :: OWLParser Statement
parseBreak = do
	breakToken
	return Break