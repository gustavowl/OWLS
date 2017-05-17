module Program where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Show.Functions
import Lexer
import Expr
import Tokens

---------------------------------------------------------------------------------------------------
-- Program (subprograms, main)
---------------------------------------------------------------------------------------------------
data Program = Program [Function] [Procedure] [Var] Function
instance Show Program where
	show(Program f p v m) = "Program { \n" ++
		"Functions: " ++ show f ++ "\n" ++ 
		"Procedures: " ++ show p ++ "\n" ++
		"GlobalVar: " ++ show v ++ "\n" ++ 
		"Main: " ++ show m ++ "\n}"

---------------------------------------------------------------------------------------------------
-- Function (parameters, return type, body)
---------------------------------------------------------------------------------------------------
data Function = Function TokenType [Var] TokenType [Statement] 
instance Show Function where
	show(Function id p r b) = "Function { \n" ++ 
		"\tName: " ++ show id ++ "\n" ++ 
		"\tParam: " ++ show p ++ "\n" ++ 
		"\tReturn Type: " ++ show r ++ "\n" ++ 
		"\tBody: " ++ show b ++ "\n}"

---------------------------------------------------------------------------------------------------
-- Procedure (parameters, body)
---------------------------------------------------------------------------------------------------
data Procedure = Procedure TokenType [Var] [Statement] 
instance Show Procedure where
	show(Procedure n p b) = "Procedure { \n" ++ 
		"\tName: " ++ show id ++ "\n" ++ 
		"\tParam: " ++ show p ++ "\n" ++ 
		"\tBody: " ++ show b ++ "\n}"

---------------------------------------------------------------------------------------------------
-- Var (name, type)
---------------------------------------------------------------------------------------------------
data Var = Var TokenType TokenType -- TODO
instance Show Var where
	show(Var n t) = "Var { \n" ++ 
		"\tName: " ++ show n ++ "\n" ++ 
		"\tType: " ++ show t ++ "\n}"

---------------------------------------------------------------------------------------------------
-- Statements - Condition / While / For / Attribuition / Comp_Attribuition
---------------------------------------------------------------------------------------------------
-- Statement (genelarization)
data Statement =  
	-- Condition (expression, block) 
	Condition Expr [Statement]
	-- Switch
	| SwitchCase Expr [(Expr, [Statement])]
	-- Attribuition (id, expression)
	| Assignment TokenType Expr
--	| While BoolExpr [Statement]
--	| For BoolExpr [Statement]
--	| Attribuition Var Expr
--	| CAttr
instance Show Statement where
	show (Condition a b) = "If{" ++ show a ++ "," ++ show b ++ "}\n"
	show (Assignment a b) = "Assign{" ++ show a ++ "," ++ show b ++ "}\n" 

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseOWLS :: String -> Either ParseError Program
parseOWLS input = runParser parseProgram (OWLState []) "" (getTokens input)

parseProgram :: OWLParser Program
parseProgram = do
	v <- parseGlobalVariables
	f <- many parseFunction
	p <- many parseProcedure
	m <- parseMain
	return $ Program f p v m

parseMain :: OWLParser Function
parseMain = do
	id <- mainToken
	params <- parseParameters
	body <- parseBlock
	return $ Function id params (Id "char[]") body

parseFunction :: OWLParser Function
parseFunction =  do
	funcToken
	id <- identifier
	params <- parseParameters
	ret <- parseReturnType
	body <- parseBlock
	return $ Function id params ret body 

parseProcedure :: OWLParser Procedure
parseProcedure = do
	procToken
	id <- identifier
	params <- parseParameters
	body <- parseBlock
	return $ Procedure id params body 

parseGlobalVariables :: OWLParser [Var]
parseGlobalVariables = endBy (try parseVariable) semi

parseParameters :: OWLParser [Var]
parseParameters = parens (sepBy parseVariable comma)

parseVariable :: OWLParser Var
parseVariable = do
	name <- identifier
	colon
	t <- identifier
	return $ Var name t

parseReturnType :: OWLParser TokenType
parseReturnType = do
	colon
	t <- identifier
	return t

parseBlock :: OWLParser [Statement]
parseBlock = braces (many parseStatement)

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

parseStatement :: OWLParser Statement
parseStatement = (try parseAssignment) 
	<|> (try parseCondition) 
	<|> (try parseSwitchCase) 
	-- TODO: other statement types

parseCondition :: OWLParser Statement
parseCondition = do
	ifToken
	exp <- parseExpr
	block <- parseBlock
	return $ Condition exp block

parseAssignment :: OWLParser Statement
parseAssignment = do
	id <- identifier
	assignToken
	exp <- parseExpr
	semi
	return $ Assignment id exp

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