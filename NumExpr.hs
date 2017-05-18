module Expr where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Lexer
import Tokens

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseNumExpr :: OWLInterpreter
parseNumExpr = parseOrChain

parseExprNumLeaf :: OWLInterpreter
parseExprNumLeaf = (try parseNumUnary)
	<|> (try parseNumber)
	<|> (try (parens parseNumExpr))
	<|> (try parseNumFuncCall)
	<|> parseNumID

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumID :: OWLInterpreter
parseNumID = do
	id <- identifier
	k <- getVarKey id
	((_, t), v) <- getVar k
	if checkType t (NumberType "nat") then
		return (NumberType "nat", v)
	else if checkType t (AtomicType "int") then
		return (NumberType "int", v)
	else if checkType t (AtomicType "real") then
		return (NumberType "real", v)
	else
		fail "Variable value " ++ id ++ " is not a number."
	return v

---------------------------------------------------------------------------------------------------
-- Grammar - Literal Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumber :: OWLInterpreter
parseNumber = parseNatural <|> parseInteger <|> parseReal

parseNatural :: OWLInterpreter
parseNatural = do
	n <- natural
	return (AtomicType "nat", NumberValue n)

parseInteger :: OWLInterpreter
parseInteger = do
	n <- integer
	return (AtomicType "int", NumberValue n)

parseReal :: OWLInterpreter
parseReal = do
	n <- real
	return (AtomicType "real", NumberValue n)

---------------------------------------------------------------------------------------------------
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseNumUnary :: OWLInterpreter
parseNumUnary = parseMinus

parseMinus :: OWLInterpreter
parseMinus = do
	op <- minusToken
	parseExprNumLeaf >>= f where
	f (t, NumberValue v) = do 
		if t = (AtomicType "nat") then 
			return (AtomicType "int", NumberValue -v)
		else
			return (t, NumberValue -v)
	f _ = fail "WTF"

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