module Program where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Show.Functions
import Lexer
import Expr

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
-- Statements - If / While / For / Attribuition / Comp_Attribuition
---------------------------------------------------------------------------------------------------
-- Statement (genelarization)
data Statement =  
	-- If (expression, block) 
	If Expr [Statement]
	-- Attribuition (id, expression)
	| Attr String Expr
--	  If BoolExpr [Statement]
--	| While BoolExpr [Statement]
--	| For BoolExpr [Statement]
--	| Attribuition Var Expr
--	| CAttr
instance Show Statement where
	show (If a b) = "If{" ++ show a ++ "," ++ "}"
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
parseBlock = braces (endBy parseStatement semi)

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

parseStatement :: Parser Statement
parseStatement = (try parseAttr) <|> (try parseIf) -- TODO: other statement types

parseIf :: Parser Statement
parseIf = do
	reserved "if"
	exp <- parseExpr
	block <- parseBlock
	return $ If exp block

parseAttr :: Parser Statement
parseAttr = do
	id <- identifier
	reservedOp "=" 
	exp <- parseExpr
	return $ Attr id exp
