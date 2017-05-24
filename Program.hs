module Program where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Show.Functions
import State
import Lexer
import Tokens
import Expr
import NumExpr
import BoolExpr
import FuncCall

-------------------------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------------------------

parseOWLS :: String -> Either ParseError Double
parseOWLS input = runParser parseProgram initState "" (alexScanTokens input)

-- Executes program
parseProgram :: OWLParser Double
parseProgram = do
	many parseDeclaration
	m <- parseMain
	return m

-- Executes main
parseMain :: OWLParser Double
parseMain = do
	mainToken
	params <- parseParamList
	parseBlock (AtomicType "int") >>= f where
		f (Just (t, n)) =
			if checkType t (AtomicType "int") then
				return $ getNumber n
			else
				fail $ "Could not convert " ++ show t ++ " to int."
		f _ = return 0

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------

parseVarDec :: OWLInterpreter
parseVarDec = do
	name <- identifier
	colon
	t <- parseVarType
	semi
	updateState (addVar (name, 0, t))
	return Nothing
{-
parseFuncDec :: OWLInterpreter
parseFuncDec = do
	funcToken
	id <- identifier
	params <- parseParamList
	colon
	ret <- identifier
	updateState (addFunc (id, params, ret))
	try (parseFuncDef id) <|> semi
	return Nothing

parseFuncDef :: String -> OWLInterpreter
parseFuncDef id = do
	body <- braces
	updateState (updateFunc (id, body))
	return Nothing

parseProcDec :: OWLInterpreter
parseProcDec = do
	procToken
	id <- identifier
	params <- parseParamList
	updateState (addProc (id, params))
	body <- parseBlock
	try (parseProcDef id) <|> semi
	return Nothing

parseProcDef :: String -> OWLInterpreter
parseProcDef id = do
	body <- braces 
	updateState $ updateProc (id, body)
	return Nothing
-}
parseParamList :: OWLParser [VarDec]
parseParamList = parens (sepBy parseParamDec comma)

parseParamDec :: OWLParser VarDec
parseParamDec = do
	name <- identifier
	colon
	t <- parseVarType
	return (name, 0, t)

parseVarType :: OWLParser VarType
parseVarType = do
	id <- identifier
	return $ AtomicType id -- TODO

---------------------------------------------------------------------------------------------------
-- Statements execution
---------------------------------------------------------------------------------------------------

parseBlock :: VarType -> OWLInterpreter
parseBlock t = braces $ parseStatements t

parseStatements :: VarType -> OWLInterpreter
parseStatements t = do
	(optionMaybe $ parseStatement t) >>= stmt where
		stmt (Just (Just a)) = return $ Just a
		stmt (Just Nothing) = do
			s <- parseStatements t
			return s
		stmt _ = return Nothing

parseStatement :: VarType -> OWLInterpreter
parseStatement t = (try $ parseReturn t)
	<|> (try parseDeclaration) 
	<|> (try parseAssignment)
	<|> (try $ parseCondition t)
	-- TODO: other statement types

parseReturn :: VarType -> OWLInterpreter
parseReturn t = do
	returnToken
	value <- parseExpr t
	semi
	return $ Just (t, value)

parseDeclaration :: OWLInterpreter
parseDeclaration = do 
	(try parseVarDec) 
	return Nothing
	-- <|> (try parseFuncDec) 
	-- <|> (try parseProcDec)

parseAssignment :: OWLInterpreter
parseAssignment = do
	state <- getState
	id <- identifier
	assignToken
	let scope = getVarScope state id
	let t = getVarType state (id, scope)
	value <- parseExpr t
	semi
	updateState $ updateVar (id, scope) value
	return Nothing

parseCondition :: VarType -> OWLInterpreter
parseCondition t = do
	ifToken
	value <- parseBoolExpr
	if value then 
		parseBlock t >>= return
	else
		return Nothing

{-
parseSwitchCase :: OWLParser Statement
parseSwitchCase = do
	switchToken
	expr <- parseExpr
	cases <- many parseCase
	return $ SwitchCase expr cases

parseCase :: OWLParser (Expr, [Statement])
parseCase = do
	caseToken
	expr <- parseExpr
	block <- parseBlock
	return $ (expr, block)
-}