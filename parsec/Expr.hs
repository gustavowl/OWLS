module Expr where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Lexer

---------------------------------------------------------------------------------------------------
-- Generic Expression
---------------------------------------------------------------------------------------------------
data Expr = ExprLeaf Term | ExprUnOp UnOp Expr | ExprBinOp BinOp Expr Expr
instance Show Expr where
	show (ExprLeaf s) = show s
	show (ExprUnOp op e) = "(" ++ show op ++ show e ++ ")"
	show (ExprBinOp op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

---------------------------------------------------------------------------------------------------
-- Leaf Terms
---------------------------------------------------------------------------------------------------
-- Number, ID or Function call with expressions as arguments
data Term = TermNat Integer | TermInt Integer | TermReal Double 
			| TermID String | TermFunc String [Expr]
instance Show Term where
	show (TermNat n) = show n
	show (TermInt n) = show n
	show (TermReal n) = show n
	show (TermID s) = s
	show (TermFunc s e) = show s ++ show e

---------------------------------------------------------------------------------------------------
-- Unary Operators
---------------------------------------------------------------------------------------------------
data UnOp = Minus | Not
instance Show UnOp where
	show (Not) = "not"
	show (Minus) = "-"

---------------------------------------------------------------------------------------------------
-- Binary Operators
---------------------------------------------------------------------------------------------------
data BinOp = Add | Sub | Mul | Div | And | Or
instance Show BinOp where
	show (Add) = "+"
	show (Sub) = "-"
	show (Mul) = "*"
	show (Div) = "/"
	show (And) = "and"
	show (Or) = "or"

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseExpr :: Parser Expr
parseExpr = parseExprLeaf <|> (parens parseExpr)-- <|> parseExprUnOp <|> parseExprBinOp

parseExprLeaf :: Parser Expr
parseExprLeaf = do
	t <- parseTerm
	return $ ExprLeaf t

---------------------------------------------------------------------------------------------------
-- Grammar - End Terms
---------------------------------------------------------------------------------------------------

parseTerm :: Parser Term
parseTerm = try parseNumber <|> try parseFuncCall <|> parseID

parseID :: Parser Term
parseID = do
	id <- identifier
	return $ TermID id

parseFuncCall :: Parser Term
parseFuncCall = do
	id <- identifier
	args <- parseArguments
	return $ TermFunc id args

parseArguments :: Parser [Expr]
parseArguments = parens (sepBy parseExpr comma)

parseNumber :: Parser Term
parseNumber = parseNaturalTerm <|> parseIntegerTerm <|> parseRealTerm

parseNaturalTerm :: Parser Term
parseNaturalTerm = do
	n <- natural
	return $ TermInt n

parseIntegerTerm :: Parser Term
parseIntegerTerm = do
	n <- integer
	return $ TermNat n

parseRealTerm :: Parser Term
parseRealTerm = do
	n <- float
	return $ TermReal n

{-
parseNumExpr :: Parser Expr
parseNumExpr = do
	
	return $ Expr terms

parseNumTerm :: Parser NumTerm
parseNumTerm = do
	endTerms <- chainl1 parseNumLeaf mul_op
	return $ NumTerm endTerms

parseNumLeaf :: Parser NumEndTerm
parseNumLeaf = parens parseExpr
	<|> integer
	<|> identifier
	<|> parseFuncCall

add_op = do { symbol "+"; return (+) }
	<|> do { symbol "-"; return (-) }

mul_op = do { symbol "*"; return (*) }
	<|> do { symbol "/"; return (div) }

neg_op = do { symbol "-"; return (-) }
-}