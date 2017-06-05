module Expr where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Lexer
import ProgramTree
import FuncCall
import qualified Tokens

---------------------------------------------------------------------------------------------------
-- General Expr
---------------------------------------------------------------------------------------------------

parseExpr :: OWLParser Expr
parseExpr = (try parseNumExpr) <|> (try parseBoolExpr) <|> parseStuffExpr

---------------------------------------------------------------------------------------------------
-- Grammar - Numeric
---------------------------------------------------------------------------------------------------

parseNumExpr :: OWLParser Expr
parseNumExpr = do
	expr <- parseAddChain
	return $ NumExpr expr

parseNumLeaf :: OWLParser NumNode
parseNumLeaf = (try parseNumUnary)
	<|> (try parseNumber)
	<|> (try (parens parseAddChain))
	<|> (try parseNumFuncCall)
	<|> parseNumID

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumID :: OWLParser NumNode
parseNumID = do
	name <- identifier
	return $ NumID name

parseNumFuncCall :: OWLParser NumNode
parseNumFuncCall = do
	(name, params) <- parseFuncCall
	return $ NumFuncCall name params

---------------------------------------------------------------------------------------------------
-- Grammar - Literal Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumber :: OWLParser NumNode
parseNumber = parseNatural <|> parseInteger <|> parseReal

parseNatural :: OWLParser NumNode
parseNatural = do
	n <- natural
	return $ NumNat n

parseInteger :: OWLParser NumNode
parseInteger = do
	n <- integer
	return $ NumInt n

parseReal :: OWLParser NumNode
parseReal = do
	n <- real
	return $ NumReal n

---------------------------------------------------------------------------------------------------
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseNumUnary :: OWLParser NumNode
parseNumUnary = parseMinus

parseMinus :: OWLParser NumNode
parseMinus = do
	op <- minusToken
	expr <- parseNumLeaf
	return $ NumMinus expr

---------------------------------------------------------------------------------------------------
-- Gramar - Binary Operators
---------------------------------------------------------------------------------------------------

parseAddChain :: OWLParser NumNode
parseAddChain = (try parseAddChainTail) <|> (try parseMulChain)

parseAddChainTail :: OWLParser NumNode
parseAddChainTail = do
	e1 <- parseMulChain
	op <- plusToken <|> minusToken
	e2 <- parseAddChain
	if op == Tokens.Plus then
		return $ NumAdd e1 e2
	else
		return $ NumSub e1 e2

parseMulChain :: OWLParser NumNode
parseMulChain = (try parseMulChainTail) <|> (try parseNumLeaf)

parseMulChainTail :: OWLParser NumNode
parseMulChainTail = do
	e1 <- parseNumLeaf
	op <- timesToken <|> divideToken <|> modulusToken
	e2 <- parseMulChain
	if op == Tokens.Times then
		return $ NumMul e1 e2
	else if op == Tokens.Divide then
		return $ NumDiv e1 e2
	else
		return $ NumMod e1 e2

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseBoolExpr :: OWLParser Expr
parseBoolExpr = do
	expr <- parseOrChain
	return $ BoolExpr expr

parseBoolLeaf :: OWLParser BoolNode
parseBoolLeaf = (try parseBoolUnary)
	<|> (try parseBoolean)
	<|> (try (parens parseOrChain))
	<|> (try parseBoolFuncCall)
	<|> parseBoolID

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseBoolean :: OWLParser BoolNode
parseBoolean = do
	b <- boolean
	return $ BoolLit b

parseBoolID :: OWLParser BoolNode
parseBoolID = do
	name <- identifier
	return $ BoolID name

parseBoolFuncCall :: OWLParser BoolNode
parseBoolFuncCall = do
	(name, params) <- parseFuncCall
	return $ BoolFuncCall name params

---------------------------------------------------------------------------------------------------
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseBoolUnary :: OWLParser BoolNode
parseBoolUnary = parseNeg

parseNeg :: OWLParser BoolNode
parseNeg = do
	notToken
	n <- parseBoolLeaf
	return $ BoolNot n

---------------------------------------------------------------------------------------------------
-- Gramar - Binary Operators
---------------------------------------------------------------------------------------------------

parseOrChain :: OWLParser BoolNode
parseOrChain = (try parseOrChainTail) <|> (try parseAndChain)

parseOrChainTail :: OWLParser BoolNode
parseOrChainTail = do
	e1 <- parseAndChain
	op <- orToken <|> corToken
	e2 <- parseOrChain
	if op == Tokens.Or then
		return $ BoolOr e1 e1
	else
		return $ BoolOrC e1 e2

parseAndChain :: OWLParser BoolNode
parseAndChain = (try parseAndChainTail) <|> (try parseEqChain)

parseAndChainTail :: OWLParser BoolNode
parseAndChainTail = do
	e1 <- parseEqChain
	op <- andToken <|> candToken
	e2 <- parseAndChain
	if op == Tokens.And then
		return $ BoolAnd e1 e1
	else
		return $ BoolAndC e1 e2

---------------------------------------------------------------------------------------------------
-- Gramar - Relational Binary Operators
---------------------------------------------------------------------------------------------------

parseEqChain :: OWLParser BoolNode
parseEqChain = (try parseEqChainTail) <|> (try parseRelational) <|> (try parseBoolLeaf)

parseEqChainTail :: OWLParser BoolNode
parseEqChainTail = do
	e1 <- parseRelational
	op <- eqToken <|> difToken
	e2 <- parseEqChain
	if op == Tokens.Equals then
		return $ BoolEq e1 e2
	else
		return $ BoolDif e1 e2

parseRelational :: OWLParser Expr
parseRelational = do
	e1 <- parseExpr
	op <- greaterToken <|> greaterEqToken <|> lessToken <|> lessEqToken
	e2 <- parseExpr
	if op == Tokens.Greater then
		return $ Expr $ BoolGt e1 e2
	else if op == Tokens.Less then
		return $ Expr $ BoolLt e1 e2
	else if op == Tokens.GreaterEq then
		return $ Expr $ BoolGtEq e1 e2
	else
		return $ Expr $ BoolLtEq e1 e2

---------------------------------------------------------------------------------------------------
-- Grammar - Stuff
---------------------------------------------------------------------------------------------------

parseStuffExpr :: OWLParser Expr
parseStuffExpr = do
	expr <- (try parseStuffFuncCall) <|> parseStuffID
	return $ StuffExpr expr

parseStuffID :: OWLParser StuffNode
parseStuffID = do
	name <- identifier
	return $ StuffID name

parseStuffFuncCall :: OWLParser StuffNode
parseStuffFuncCall = do
	(name, params) <- parseFuncCall
	return $ StuffFuncCall name params

---------------------------------------------------------------------------------------------------
-- Grammar - Stuff
---------------------------------------------------------------------------------------------------

parseFuncCall :: OWLParser (String, [Expr])
parseFuncCall = do
	return ("", [])