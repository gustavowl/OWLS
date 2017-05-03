module Program where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Lexer

-- Program (subprograms, main)
data Program = Program [Subprogram] Subprogram

-- Generalization
-- Function (parameters, return type, body)
-- Procedure (parameters, body)
data Subprogram = Function [Var] Type [Statement] | Procedure [Var] [Statement]

data Statement = Statement String -- TODO

-- Var (name, type)
data Var = Var String String -- TODO

-- Type (name)
data Type = Type String

parseSubprograms :: Parser [Subprogram]
parseSubprograms = many parseSubprogram

parseSubprogram :: Parser Subprogram
parseSubprogram = parseFunction <|> parseProcedure

parseFunction :: Parser Subprogram
parseFunction =  do
	reserved "func"
	params <- parseParameters
	ret <- parseReturnType
	body <- parseBlock
	return $ Function params ret body 

parseProcedure :: Parser Subprogram
parseProcedure = do
	reserved "proc"
	params <- parseParameters
	body <- parseBlock
	return $ Procedure params body 

parseParameters :: Parser [Var]
parseParameters = parens (sepBy parseParameter comma)

parseParameter :: Parser Var
parseParameter = do
	name <- identifier
	colon
	t <- identifier
	return $ Var name t

parseReturnType :: Parser Type
parseReturnType = do
	t <- identifier
	return $ Type t

parseBlock :: Parser [Statement]
parseBlock = braces (sepBy parseStatement semi)

parseStatement :: Parser Statement
parseStatement = do
	bla <- identifier -- TODO
	return $ Statement bla
