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

parseVarDec :: Bool -> OWLInterpreter
parseVarDec update = do
	name <- identifier
	colon
	t <- parseVarType
	semi
	if update then
		updateState (addVar (name, 0, t))
	return Nothing

parseFuncDec :: Bool -> OWLInterpreter
parseFuncDec update = do
	funcToken
	name <- identifier
	params <- parseParamList
	colon
	ret <- parseVarType
	if update then
		updateState (addFunc (name, -1, params, ret))
	try $ parseFuncDef name (-1)
	{-semi-}
	return Nothing

parseFuncDef :: String -> Integer -> Bool -> OWLInterpreter
parseFuncDef name scope update = do
	body <- braces (many tokenbla)
	state <- getState
	if update then
		updateState (updateFunc (name, scope) body)
	return Nothing

{-
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
parseParamList :: Bool -> OWLParser [VarDec]
parseParamList update = parens (sepBy parseParamDec update comma)

parseParamDec :: Bool -> OWLParser VarDec
parseParamDec update = do
	name <- identifier
	colon
	t <- parseVarType update
	return (name, 0, t)

parseVarType :: Bool -> OWLParser VarType
parseVarType update = do
	id <- identifier;
	-- TODO: verificar se o tipo existe 
	-- TODO: parar de ignorar os outros tipos
	return $ AtomicType id 

---------------------------------------------------------------------------------------------------
-- Statements execution
---------------------------------------------------------------------------------------------------

-- VarType : tipo esperado de retorno 
parseBlock :: VarType -> Bool -> OWLInterpreter
parseBlock t update = braces $ parseStatements t update

parseStatements :: VarType -> Bool -> OWLInterpreter
parseStatements t update = do
	(optionMaybe $ parseStatement t update) >>= stmt where
		stmt (Just (Just a)) = return $ Just a
		stmt (Just Nothing) = do
			s <- parseStatements t update
			return s
		stmt _ = return Nothing

parseStatement :: VarType -> Bool -> OWLInterpreter
parseStatement t update = (try $ parseReturn t update)
	<|> (try parseDeclaration update) 
	<|> (try parseAssignment update)
	<|> (try $ parseCondition t update)
	-- TODO: other statement types

parseReturn :: VarType -> Bool -> OWLInterpreter
parseReturn t update = do
	returnToken
	value <- parseExpr t update
	semi
	return $ Just (t, value)

parseDeclaration :: Bool -> OWLInterpreter
parseDeclaration update = do 
	(try parseVarDec update)
	(try parseFuncDec update) 
	return Nothing
	-- <|> (try parseFuncDec) 
	-- <|> (try parseProcDec)

parseAssignment :: Bool -> OWLInterpreter
parseAssignment update = do
	state <- getState
	id <- identifier
	assignToken
	if update then
		let scope = getVarScope state id
		let t = getVarType state (id, scope)
		value <- parseExpr (Just t) update
		semi
		updateState $ updateVar (id, scope) value
	else
		parseExpr Nothing update 
		semi
	return Nothing

parseCondition :: VarType -> Bool -> OWLInterpreter
parseCondition t update = do
	ifToken
	value <- parseBoolExpr update
	body <- parseBlock t False
	if value then 
		body 
		parseBlock t update >>= return
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