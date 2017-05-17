module Expr where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Lexer
import Tokens

---------------------------------------------------------------------------------------------------
-- Generic Expression
---------------------------------------------------------------------------------------------------
data Expr = ExprToken TokenType | ExprFunc TokenType [Expr] |
			ExprUnOp UnOp Expr | ExprBinOp BinOp Expr Expr

instance Show Expr where
	show (ExprToken t) = show t
	show (ExprFunc s e) = show s ++ show e
	show (ExprUnOp op e) = "(" ++ show op ++ show e ++ ")"
	show (ExprBinOp op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

---------------------------------------------------------------------------------------------------
-- Unary Operators
---------------------------------------------------------------------------------------------------
data UnOp = Neg | Not
instance Show UnOp where
	show (Not) = "not"
	show (Neg) = "-"

---------------------------------------------------------------------------------------------------
-- Binary Operators
---------------------------------------------------------------------------------------------------
data BinOp = Add | Sub | Mul | Div | Mod | Conj | CConj | Disj | CDisj | Eq | Dif | Lt | Gt | Lte | Gte
instance Show BinOp where
	show (Add) = "+"
	show (Sub) = "-"
	show (Mul) = "*"
	show (Div) = "/"
	show (Mod) = "%"
	show (Conj) = "and"
	show (CConj) = "cand"
	show (Disj) = "or"
	show (CDisj) = "cor"
	show (Eq) = "=="
	show (Dif) = "!="
	show (Lt) = "<"
	show (Gt) = ">"
	show (Lte) = "<="
	show (Gte) = ">="

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseExpr :: OWLParser Expr
parseExpr = parseOrChain

parseExprLeaf :: OWLParser Expr
parseExprLeaf = (try parseUnary)
	<|> (try parseLiteral)
	<|> (try parseFuncCall)
	<|> (try parseID)
	<|> (try (parens parseExpr))

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseID :: OWLParser Expr
parseID = do
	id <- identifier
	return $ ExprToken id

parseFuncCall :: OWLParser Expr
parseFuncCall = do
	id <- identifier
	args <- parseArguments
	return $ ExprFunc id args

parseArguments :: OWLParser [Expr]
parseArguments = parens (sepBy parseExpr comma)

parseLiteral :: OWLParser Expr
parseLiteral = parseNumber <|> parseBool

---------------------------------------------------------------------------------------------------
-- Grammar - Literal Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumber :: OWLParser Expr
parseNumber = parseNatural <|> parseInteger <|> parseReal

parseNatural :: OWLParser Expr
parseNatural = do
	n <- natural
	return $ ExprToken n

parseInteger :: OWLParser Expr
parseInteger = do
	n <- integer
	return $ ExprToken n

parseReal :: OWLParser Expr
parseReal = do
	n <- real
	return $ ExprToken n

parseBool :: OWLParser Expr
parseBool = do
	n <- boolean
	return $ ExprToken n

---------------------------------------------------------------------------------------------------
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseUnary :: OWLParser Expr
parseUnary = parseMinus <|> parseNot

parseMinus :: OWLParser Expr
parseMinus = do
	op <- try parseMinusOp
	e <- try parseExprLeaf
	return $ ExprUnOp op e

parseMinusOp :: OWLParser UnOp
parseMinusOp = do
	minusToken
	return $ Neg

parseNot :: OWLParser Expr
parseNot = do
	op <- try parseNotOp
	e <- try parseExprLeaf
	return $ ExprUnOp op e

parseNotOp :: OWLParser UnOp
parseNotOp = do
	exclamationToken
	return $ Not

---------------------------------------------------------------------------------------------------
-- Gramar - Logic Binary Operators
---------------------------------------------------------------------------------------------------

parseOrChain :: OWLParser Expr
parseOrChain = (try parseOrChainTail) <|> (try parseAndChain)

parseOrChainTail :: OWLParser Expr
parseOrChainTail = do
	e1 <- parseAndChain
	op <- parseOrOp <|> parseCOrOp
	e2 <- parseOrChain
	return $ ExprBinOp op e1 e2

parseOrOp :: OWLParser BinOp
parseOrOp = do
	orToken
	return $ Disj

parseCOrOp :: OWLParser BinOp
parseCOrOp = do
	corToken
	return $ CDisj

parseAndChain :: OWLParser Expr
parseAndChain = (try parseAndChainTail) <|> (try parseEqChain)

parseAndChainTail :: OWLParser Expr
parseAndChainTail = do
	e1 <- parseEqChain
	op <- parseAndOp <|> parseCAndOp
	e2 <- parseAndChain
	return $ ExprBinOp op e1 e2

parseAndOp :: OWLParser BinOp
parseAndOp = do
	andToken
	return $ Conj

parseCAndOp :: OWLParser BinOp
parseCAndOp = do
	candToken
	return $ CConj

---------------------------------------------------------------------------------------------------
-- Gramar - Relational Binary Operators
---------------------------------------------------------------------------------------------------

parseEqChain :: OWLParser Expr
parseEqChain = (try parseEqChainTail) <|> (try parseRelational)

parseEqChainTail :: OWLParser Expr
parseEqChainTail = do
	e1 <- parseRelational
	op <- parseEqualsOp <|> parseDiffersOp
	e2 <- parseEqChain
	return $ ExprBinOp op e1 e2

parseEqualsOp :: OWLParser BinOp
parseEqualsOp = do
	eqToken
	return $ Eq

parseDiffersOp :: OWLParser BinOp
parseDiffersOp = do
	difToken
	return $ Dif

parseRelational :: OWLParser Expr
parseRelational = (try parseRelationalTail) <|> (try parseAddChain)

parseRelationalTail :: OWLParser Expr
parseRelationalTail = do
	e1 <- parseAddChain
	op <- parseLtOp <|> parseGtOp <|> parseLteOp <|> parseGteOp
	e2 <- parseAddChain
	return $ ExprBinOp op e1 e2

parseLtOp :: OWLParser BinOp
parseLtOp = do
	lessToken
	return $ Lt

parseGtOp :: OWLParser BinOp
parseGtOp = do
	greaterToken
	return $ Gt

parseLteOp :: OWLParser BinOp
parseLteOp = do
	lessEqToken
	return $ Lte

parseGteOp :: OWLParser BinOp
parseGteOp = do
	greaterEqToken
	return $ Gte

---------------------------------------------------------------------------------------------------
-- Gramar - Numeric Binary Operators
---------------------------------------------------------------------------------------------------

parseAddChain :: OWLParser Expr
parseAddChain = (try parseAddChainTail) <|> (try parseMulChain)

parseAddChainTail :: OWLParser Expr
parseAddChainTail = do
	e1 <- parseMulChain
	op <- parseAddOp <|> parseSubOp
	e2 <- parseAddChain
	return $ ExprBinOp op e1 e2

parseAddOp :: OWLParser BinOp
parseAddOp = do
	plusToken
	return $ Add

parseSubOp :: OWLParser BinOp
parseSubOp = do
	minusToken
	return $ Sub

parseMulChain :: OWLParser Expr
parseMulChain = (try parseMulChainTail) <|> (try parseExprLeaf)

parseMulChainTail :: OWLParser Expr
parseMulChainTail = do
	e1 <- parseExprLeaf
	op <- parseMulOp <|> parseDivOp <|> parseModOp
	e2 <- parseMulChain
	return $ ExprBinOp op e1 e2

parseMulOp :: OWLParser BinOp
parseMulOp = do
	timesToken
	return $ Mul

parseDivOp :: OWLParser BinOp
parseDivOp = do
	divideToken
	return $ Div

parseModOp :: OWLParser BinOp
parseModOp = do
	modulusToken
	return $ Mod