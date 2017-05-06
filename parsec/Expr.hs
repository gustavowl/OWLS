module Expr where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Lexer

---------------------------------------------------------------------------------------------------
-- Generic Expression
---------------------------------------------------------------------------------------------------
data Expr = ExprNat Integer | ExprInt Integer | ExprReal Double | ExprBool Bool |
			ExprID String | ExprFunc String [Expr] |
			ExprUnOp UnOp Expr | ExprBinOp BinOp Expr Expr

instance Show Expr where
	show (ExprNat n) = show n
	show (ExprInt n) = show n
	show (ExprReal n) = show n
	show (ExprBool n) = show n
	show (ExprID s) = show s
	show (ExprFunc s e) = show s ++ show e
	show (ExprUnOp op e) = "(" ++ show op ++ show e ++ ")"
	show (ExprBinOp op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

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
parseExpr = parseBinaryChain

parseExprLeaf :: Parser Expr
parseExprLeaf = (try parseUnary)
	<|> (try parseLiteral)
	<|> (try parseFuncCall)
	<|> (try parseID)
	<|> (try (parens parseExpr))

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseID :: Parser Expr
parseID = do
	id <- identifier
	return $ ExprID id

parseFuncCall :: Parser Expr
parseFuncCall = do
	id <- identifier
	args <- parseArguments
	return $ ExprFunc id args

parseArguments :: Parser [Expr]
parseArguments = parens (sepBy parseExpr comma)

parseLiteral :: Parser Expr
parseLiteral = parseNumber <|> parseBool

---------------------------------------------------------------------------------------------------
-- Grammar - Numeric Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumber :: Parser Expr
parseNumber = parseNatural <|> parseInteger <|> parseReal

parseNatural :: Parser Expr
parseNatural = do
	n <- natural
	return $ ExprInt n

parseInteger :: Parser Expr
parseInteger = do
	n <- integer
	return $ ExprNat n

parseReal :: Parser Expr
parseReal = do
	n <- float
	return $ ExprReal n

---------------------------------------------------------------------------------------------------
-- Grammar - Boolean Leaf Terms
---------------------------------------------------------------------------------------------------

parseBool :: Parser Expr
parseBool = parseTrue <|> parseFalse

parseTrue :: Parser Expr
parseTrue = do
	reserved "true"
	return $ ExprBool True

parseFalse:: Parser Expr
parseFalse = do
	reserved "false"
	return $ ExprBool False

---------------------------------------------------------------------------------------------------
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseUnary :: Parser Expr
parseUnary = parseMinus <|> parseNot

parseMinus :: Parser Expr
parseMinus = do
	op <- try parseMinusOp
	e <- try parseExprLeaf
	return $ ExprUnOp op e

parseMinusOp :: Parser UnOp
parseMinusOp = do
	reservedOp "-"
	return $ Minus

parseNot :: Parser Expr
parseNot = do
	op <- try parseNotOp
	e <- try parseExprLeaf
	return $ ExprUnOp op e

parseNotOp :: Parser UnOp
parseNotOp = do
	reserved "not"
	return $ Not

---------------------------------------------------------------------------------------------------
-- Gramar - Binary Operators
---------------------------------------------------------------------------------------------------

parseBinaryChain :: Parser Expr
parseBinaryChain = (try parseBinaryChainTail) <|> (try parseBinaryChain')

parseBinaryChainTail :: Parser Expr
parseBinaryChainTail = do
	e1 <- parseBinaryChain'
	op <- parseAddOp <|> parseSubOp <|> parseOrOp
	e2 <- parseBinaryChain
	return $ ExprBinOp op e1 e2

parseAddOp :: Parser BinOp
parseAddOp = do
	reservedOp "+"
	return $ Add

parseSubOp :: Parser BinOp
parseSubOp = do
	reservedOp "-"
	return $ Sub

parseOrOp :: Parser BinOp
parseOrOp = do
	reserved "or"
	return $ Or

parseBinaryChain' :: Parser Expr
parseBinaryChain' = (try parseBinaryChainTail') <|> (try parseExprLeaf)

parseBinaryChainTail' :: Parser Expr
parseBinaryChainTail' = do
	e1 <- parseExprLeaf
	op <- (try parseMulOp) <|> parseDivOp <|> parseAndOp
	e2 <- parseBinaryChain'
	return $ ExprBinOp op e1 e2

parseMulOp :: Parser BinOp
parseMulOp = do
	reservedOp "*"
	return $ Mul

parseDivOp :: Parser BinOp
parseDivOp = do
	reservedOp "/"
	return $ Div

parseAndOp :: Parser BinOp
parseAndOp = do
	reserved "and"
	return $ And