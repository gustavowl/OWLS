module Program where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

-- Program (subprograms, main)
data Program = Program [Subprogram] Subprogram

-- Generalization
-- Function (parameters, return type, body)
-- Procedure (parameters, body)
data Subprogram = Function [String] String [Statement] | Procedure [String] [Statement]

data Statement = String -- TODO

parseSubprograms :: Parser [Subprogram]
parseSubprograms = many parseSubprogram

parseSubprogram :: Parser Subprogram
parseSubprogram = parseFunction <|> parseProcedure

parseFunction :: Parser Subprogram
parseFunction = do
	reserved "func"
	params <- parseParameters
	ret <- parseReturnType
	body <- parseBlock
	return $ Function params ret body 

parseProcedure :: Parser Subprogram
parseProcedure = do
	symbol "proc"
	params <- parseParameters
	body <- parseBlock
	return $ Procedure params body 

parseParameters :: Parser [String]
parseParameters = sepBy parens comma

parseReturnType :: Parser String
parseReturnType = do
	colon
	return identifier

parseBlock :: Parser [Statement]
parseBlock = do 
	return sepBy braces semi

