module Expr where

import Text.ParserCombinators.Parsec

data Expr = BoolExpr
		| NumExpr

data NumExpr = NumTerm | Add AddOp NumExpr  
data NumTerm = bla
data NumLeaf

data BoolExpr = bla
data BoolTerm = bla
data BoolLeaf

parseExpr :: Parser Expr
parseExpr = parseNumExpr <|> parseBoolExpr

parseNumExpr :: Parser Expr
parseNumExpr = do
	terms <- chainl1 parseNumExpr add_op
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