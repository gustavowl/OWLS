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
	show (TermID s) = show s
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
parseExpr = (try parseAddChain) -- <|> parseOrChain

parseExprLeaf :: Parser Expr
parseExprLeaf = (try parseUnary) <|> (try (parens parseExpr)) <|> parseExprTerm

parseExprTerm :: Parser Expr
parseExprTerm = do
	t <- parseTerm
	return $ ExprLeaf t

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseTerm :: Parser Term
parseTerm = (try parseNumber) <|> (try parseFuncCall) <|> parseID

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

---------------------------------------------------------------------------------------------------
-- Gramar - Binary Operators
---------------------------------------------------------------------------------------------------

parseUnary :: Parser Expr
parseUnary = do
	op <- parseMinusOp
	e <- parseExprLeaf
	return $ ExprUnOp op e

parseMinusOp :: Parser UnOp
parseMinusOp = do
	reservedOp "-"
	return Minus

---------------------------------------------------------------------------------------------------
-- Gramar - Numeric Binary Operators
---------------------------------------------------------------------------------------------------

parseAddChain :: Parser Expr
parseAddChain = do
	e <- try parseAddChainTail <|> parseMulChain
	return e

parseAddChainTail :: Parser Expr
parseAddChainTail = do
	e1 <- parseMulChain
	op <- parseAddOp <|> parseSubOp
	e2 <- parseAddChain
	return $ ExprBinOp op e1 e2

parseAddOp :: Parser BinOp
parseAddOp = do
	reservedOp "+"
	return $ Add

parseSubOp :: Parser BinOp
parseSubOp = do
	reservedOp "-"
	return $ Sub

parseMulChain :: Parser Expr
parseMulChain = do
	e <- try parseMulChainTail <|> parseExprLeaf
	return e

parseMulChainTail :: Parser Expr
parseMulChainTail = do
	e1 <- parseExprLeaf
	op <- parseMulOp <|> parseDivOp
	e2 <- parseMulChain
	return $ ExprBinOp op e1 e2

parseMulOp :: Parser BinOp
parseMulOp = do
	reservedOp "*"
	return $ Mul

parseDivOp :: Parser BinOp
parseDivOp = do
	reservedOp "/"
	return $ Div
