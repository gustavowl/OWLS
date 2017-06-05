module BoolExpr where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import State
import Lexer
import NumExpr
import qualified Tokens

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseBoolExpr :: OWLParser Expr
parseBoolExpr = 
	expr <- parseOrChain
	return $ BoolNode expr

parseBoolLeaf :: OWLParser BoolNode
parseBoolLeaf = (try parseBoolUnary)
	<|> (try boolean)
	<|> (try (parens parseBoolExpr))
	<|> (try parseBoolFuncCall)
	<|> parseBoolID

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseBoolID :: OWLParser BoolNode
parseBoolID = do
	name <- identifier
	retun $ BoolID name

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
		return $ BinOr e1 e1
	else
		return $ BinOrC e1 e2

parseAndChain :: OWLParser BoolNode
parseAndChain = (try parseAndChainTail) <|> (try parseEqChain)

parseAndChainTail :: OWLParser BoolNode
parseAndChainTail = do
	e1 <- parseEqChain
	op <- andToken <|> candToken
	e2 <- parseAndChain
	if op == Tokens.And then
		return $ BinAnd e1 e1
	else
		return $ BinAndC e1 e2

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

parseRelational :: OWLParser BoolNode
parseRelational = do
	e1 <- parseExpr
	op <- greaterToken <|> greaterEqToken <|> lessToken <|> lessEqToken
	e2 <- parseExpr
	if op == Tokens.Greater then
		return $ BoolGt e1 e2
	else if op == Tokens.Less then
		return $ BoolLt e1 e2
	else if op == Tokens.GreaterEq then
		return $ BoolGtEq e1 e2
	else
		return $ BoolLtEq e1 e2