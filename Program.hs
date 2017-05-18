module Program where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Show.Functions
import State
import Lexer
import Expr

-------------------------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------------------------

parseOWLS :: String -> Either ParseError Program
parseOWLS input = runParser parseProgram (initState "" (getTokens input)

-- Executes program
parseProgram :: OWLParser Integer
parseProgram = do
	many parseDeclaration
	m <- parseMain
	return m

-- Executes main
parseMain :: OWLParser (Maybe VarValue)
parseMain = do
	mainToken
	parseParameters
	parseBlock >>= f where
		f (Just (t, n)) = do 
			if checkType t (AtomicType "int") then
				return n
			else
				fail "Could not convert " ++ show t ++ " to int."
		f _ = return 0

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------

parseVarDec :: OWLParser ()
parseVarDec = do
	name <- identifier
	colon
	t <- parseVarType
	semicolon
	updateState (addVar (name, t))

parseFuncDec :: OWLParser ()
parseFuncDec = do
	funcToken
	id <- identifier
	params <- parens (sepBy parseParamDec comma)
	colon
	ret <- identifier
	updateState (addFunc (id, params, ret))
	try (parseFuncDef id) <|> semi

parseFuncDef :: String -> OWLParser ()
parseFuncDef id = do
	body <- braces
	updateState (updateFunc (id, body))

parseProcDec :: OWLParser ()
parseProcDec = do
	procToken
	id <- identifier
	params <- parens (sepBy parseParamDec comma)
	updateState (addProc (id, params))
	body <- parseBlock
	try (parseProcDef id) <|> semi

parseProcDef :: String -> OWLParser ()
parseProcDef id = do
	body <- braces
	updateState (updateProc (id, body))

parseParamDec :: OWLParser VarDec
parseParamDec = do
	id <- identifier
	colon
	t <- identifier
	return $ VarDec id t

---------------------------------------------------------------------------------------------------
-- Statements execution
---------------------------------------------------------------------------------------------------

parseBlock :: OWLInterpreter
parseBlock = braces parseStatements

parseStatements :: OWLInterpreter
parseStatements = do
	try do { parseStatement >>= f where
			f (Just a) = do 
				return $ Just a
			f _ = return parseStatements }
	<|> do return Nothing

parseReturn :: OWLInterpreter
parseReturn = do
	returnToken
	exp <- parseExpr
	semi
	return $ Just exp

parseStatement :: OWLInterpreter
parseStatement = (try parseDeclaration) 
	<|> (try parseAssignment)
	<|> (try parseReturn)
	<|> (try parseCondition)
	-- TODO: other statement types

parseDeclaration :: OWLInterpreter
parseDeclaration = do
	(try parseVarDec) <|> (try parseFuncDec) <|> (try parseProcDec)
	return Nothing

parseAssignment :: OWLInterpreter
parseAssignment = do
	id <- identifier
	assignToken
	(expType, expValue) <- parseExpr
	semi
	scope <- getVarScope id
	varType <- getVarType (id, scope)
	if checkType expType varType = True then
		updateState (updateVar (id, exp))
	else
		fail "Could not convert " ++ show expType ++ " to " ++ show varType ++ "."
	return Nothing

parseCondition :: OWLInterpreter
parseCondition = do
	ifToken
	(expType, expValue) <- parseExpr
	if expType = (AtomicType "boolean") then
		if expValue = True then 
			return parseBlock
		else
			return Nothing
	else
		fail "Expression type is not boolean: " ++ show expType

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