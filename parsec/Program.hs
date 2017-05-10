module Program where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Show.Functions
import Lexer
import Expr
import qualified Tokens as T

---------------------------------------------------------------------------------------------------
-- Program (subprograms, main)
---------------------------------------------------------------------------------------------------
data Program = Program [Function] [Procedure] [Var] Function
instance Show Program where
	show(Program f p v m) = "Program{" ++
		show f ++ "," ++ show p ++ "," ++
		show v ++ "," ++ show m ++ "}"

---------------------------------------------------------------------------------------------------
-- Function (parameters, return type, body)
---------------------------------------------------------------------------------------------------
data Function = Function T.Token [Var] T.Token [Statement] 
instance Show Function where
	show(Function n p r b) = "Function{" ++ 
		show n ++ "," ++ show p ++ "," ++ 
		show r ++ "," ++ show b ++ "}"

---------------------------------------------------------------------------------------------------
-- Procedure (parameters, body)
---------------------------------------------------------------------------------------------------
data Procedure = Procedure T.Token [Var] [Statement] 
instance Show Procedure where
	show(Procedure n p b) = "Procedure{" ++ 
		show n ++ "," ++ show p ++ "," ++ show b ++ "}"

---------------------------------------------------------------------------------------------------
-- Var (name, type)
---------------------------------------------------------------------------------------------------
data Var = Var T.Token T.Token -- TODO
instance Show Var where
	show(Var n t) = "Var{" ++ show n ++ "," ++ show t ++ "}"

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
	| Assignment T.Token Expr
--	| While BoolExpr [Statement]
--	| For BoolExpr [Statement]
--	| Attribuition Var Expr
--	| CAttr
instance Show Statement where
	show (Condition a b) = "If{" ++ show a ++ "," ++ "}"
	show (Assignment a b) = "Assign{" ++ show a ++ "," ++ show b ++ "}" 

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseOWLS :: String -> Either ParseError Program
parseOWLS input = runParser parseProgram (OWLState []) "" (T.getTokens input)

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
	return $ Function id params (T.Token (T.Id "char[]") 1 1) body

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

parseReturnType :: OWLParser T.Token
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