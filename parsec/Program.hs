module Program where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Show.Functions
import Lexer -- Here is defined Token 
import Expr

data OWLParser = Parsec [Token] () Program
data Token = String

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
data Function = Function String [Var] Type [Statement] 
instance Show Function where
	show(Function n p r b) = "Function{" ++ 
		show n ++ "," ++ show p ++ "," ++ 
		show r ++ "," ++ show b ++ "}"

---------------------------------------------------------------------------------------------------
-- Procedure (parameters, body)
---------------------------------------------------------------------------------------------------
data Procedure = Procedure String [Var] [Statement] 
instance Show Procedure where
	show(Procedure n p b) = "Procedure{" ++ 
		show n ++ "," ++ show p ++ "," ++ show b ++ "}"

---------------------------------------------------------------------------------------------------
-- Var (name, type)
---------------------------------------------------------------------------------------------------
data Var = Var String String -- TODO
instance Show Var where
	show(Var n t) = "Var{" ++ show n ++ "," ++ show t ++ "}"

---------------------------------------------------------------------------------------------------
-- Type (name)
---------------------------------------------------------------------------------------------------
data Type = Type String 
instance Show Type where
	show(Type n) = show n

---------------------------------------------------------------------------------------------------
-- Statements - Condition / While / For / Attribuition / Comp_Attribuition
---------------------------------------------------------------------------------------------------
-- Statement (genelarization)
data Statement =  
<<<<<<< HEAD
	-- If (expression, block) 
	If Expr [Statement]
	-- Switch
	| Switch Expr [(Expr, [Statement])]
=======
	-- Condition (expression, block) 
	Condition Expr [Statement]
>>>>>>> 88fcfe12212412cd1795ce4ba14797e44df90307
	-- Attribuition (id, expression)
	| Attr String Expr
--	| While BoolExpr [Statement]
--	| For BoolExpr [Statement]
--	| Attribuition Var Expr
--	| CAttr
instance Show Statement where
<<<<<<< HEAD
	show (If a b) = "If{" ++ show a ++ "," ++ show b ++ "}"
=======
	show (Condition a b) = "If{" ++ show a ++ "," ++ "}"
>>>>>>> 88fcfe12212412cd1795ce4ba14797e44df90307
	show (Attr a b) = "Attr{" ++ show a ++ "," ++ show b ++ "}" 

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseProgram :: Parser Program
parseProgram = do
	v <- parseGlobalVariables
	f <- many parseFunction
	p <- many parseProcedure
	m <- parseMain
	return $ Program f p v m

parseMain :: Parser Function
parseMain = do
	reserved "main"
	params <- parseParameters
	body <- parseBlock
	return $ Function "" params (Type "char[]") body

parseFunction :: Parser Function
parseFunction =  do
	reserved "func"
	id <- identifier
	params <- parseParameters
	ret <- parseReturnType
	body <- parseBlock
	return $ Function id params ret body 

parseProcedure :: Parser Procedure
parseProcedure = do
	reserved "proc"
	id <- identifier
	params <- parseParameters
	body <- parseBlock
	return $ Procedure id params body 

parseGlobalVariables :: Parser [Var]
parseGlobalVariables = endBy (try parseVariable) semi

parseParameters :: Parser [Var]
parseParameters = parens (sepBy parseVariable comma)

parseVariable :: Parser Var
parseVariable = do
	name <- identifier
	colon
	t <- identifier
	return $ Var name t

parseReturnType :: Parser Type
parseReturnType = do
	colon
	t <- identifier
	return $ Type t

parseBlock :: Parser [Statement]
parseBlock = braces (many parseStatement)

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

parseStatement :: Parser Statement
parseStatement = (try parseAttr) <|> (try parseCondition) <|> (tryParseSwitch) -- TODO: other statement types

parseCondition :: Parser Statement
parseCondition = do
	reserved "if"
	exp <- parseExpr
	block <- parseBlock
	return $ Condition exp block

parseAttr :: Parser Statement
parseAttr = do
	id <- identifier
	reservedOp "=" 
	exp <- parseExpr
	semi
	return $ Attr id exp

parseSwitch :: Parser Statement
parseSwitch = do
	reserved "switch"
	expr <- parseExpr
	cases <- many parseCase
	return $ Switch expr cases

parseCase :: Parser (Expr, [Statement])
parseCase = do
	reserved "case"
	expr <- parseExpr
	block <- parseBlock
	return $ (expr, block)
{-
switch expr {
	case expr {

	}
}
-}
