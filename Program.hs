module Program where

import ProgramTree
import Expr

---------------------------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------------------------

parseOWLS :: String -> Either ParseError Program
parseOWLS input = runParser parseProgram () "" (alexScanTokens input)

parseProgram :: OWLParser Program
parseProgram = do
	dec <- many parseDeclaration
	main <- parseMain
	return $ Program dec main

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------

parseDeclaration :: OWLParser Declaration
parseDeclaration = (try parseVarDec) <|> (try parseFuncDec) <|> parseProcDec

parseMain :: OWLParser 
parseMain = do
	mainToken
	params <- parseParamList
	body <- parseBlock <|> parseEmptyBlock
	return $ Function name params (AtomicType "int") body

parseVarDec :: OWLParser Declaration
parseVarDec = do
	name <- identifier
	colon
	t <- parseVarType
	semi
	return $ Var name t

parseFuncDec :: Bool -> OWLInterpreter
parseFuncDec update = do
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

parseProcDec :: OWLInterpreter
parseProcDec = do
	procToken
	name <- identifier
	params <- parseParamList
	body <- parseBlock <|> parseEmptyBlock
	return $ Procedure name params body

parseParamList :: OWLParser [Declaration]
parseParamList = parens (sepBy parseParamDec comma)

parseParamDec :: OWLParser VarDec
parseParamDec = do
	name <- identifier
	colon
	t <- parseVarType
	return $ Var name t

parseVarType :: OWLParser VarType
parseVarType = do
	id <- identifier
	-- TODO: verificar se o tipo existe 
	-- TODO: parar de ignorar os outros tipos
	return $ AtomicType id 

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

-- VarType : tipo esperado de retorno 
parseBlock :: OWLParser [Statement]
parseBlock = braces $ many parseStatement

parseStatement :: OWLParser Statement
parseStatement = (try parseReturn)
	<|> (try parseDeclaration) 
	<|> (try parseAssignment)
	<|> (try parseCondition)
	-- TODO: other statement types

parseReturn :: OWLParser Statement
parseReturn = do
	returnToken
	expr <- parseExpr
	semi
	return $ Return expr

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
	return Nothing

parseElse :: OWLParser [Statement]
parseElse = do
	elsetoken
	body <- parseBlock
	return body

parseEmptyElse :: OWLParser [Statement]
parseEmptyElse = do
	return []
