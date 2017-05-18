module Lexer where

import Data.Functor.Identity
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import Tokens
import State

---------------------------------------------------------------------------------------------------
-- Program key words
---------------------------------------------------------------------------------------------------

mainToken :: LexParser
mainToken = tokenPrim show updatePos (simpleGetToken Main)

funcToken :: LexParser
funcToken = tokenPrim show updatePos (simpleGetToken Func)

procToken :: LexParser
procToken = tokenPrim show updatePos (simpleGetToken Proc)

---------------------------------------------------------------------------------------------------
-- Statement key words
---------------------------------------------------------------------------------------------------

ifToken :: LexParser
ifToken = tokenPrim show updatePos (simpleGetToken If)

elseToken :: LexParser
elseToken = tokenPrim show updatePos (simpleGetToken Else)

switchToken :: LexParser
switchToken = tokenPrim show updatePos (simpleGetToken Switch)

caseToken :: LexParser
caseToken = tokenPrim show updatePos (simpleGetToken Case)

returnToken :: LexParser
returnToken = tokenPrim show updatePos (simpleGetToken Return)

---------------------------------------------------------------------------------------------------
-- Characters / symbols
---------------------------------------------------------------------------------------------------

comma :: LexParser
comma = tokenPrim show updatePos (simpleGetToken Comma)

colon :: LexParser
colon = tokenPrim show updatePos (simpleGetToken Colon)

semi :: LexParser
semi = tokenPrim show updatePos (simpleGetToken SemiColon)

lparen :: LexParser
lparen = tokenPrim show updatePos (simpleGetToken LParen)

rparen :: LexParser
rparen = tokenPrim show updatePos (simpleGetToken RParen)

parens :: OWLParser a -> OWLParser a
parens p = do
	lparen
	s <- p
	rparen
	return $ s

lbrace :: LexParser
lbrace = tokenPrim show updatePos (simpleGetToken LBrace)

rbrace :: LexParser
rbrace = tokenPrim show updatePos (simpleGetToken RBrace)

braces :: OWLParser a -> OWLParser a
braces p = do
	lbrace
	s <- p
	rbrace
	return $ s

---------------------------------------------------------------------------------------------------
-- Operators
---------------------------------------------------------------------------------------------------

plusToken :: LexParser
plusToken = tokenPrim show updatePos (simpleGetToken Plus)

minusToken :: LexParser
minusToken = tokenPrim show updatePos (simpleGetToken Minus)

timesToken :: LexParser
timesToken = tokenPrim show updatePos (simpleGetToken Times)

divideToken :: LexParser
divideToken = tokenPrim show updatePos (simpleGetToken Divide)

modulusToken :: LexParser
modulusToken = tokenPrim show updatePos (simpleGetToken Modulus)

assignToken :: LexParser
assignToken = tokenPrim show updatePos (simpleGetToken Assign)

exclamationToken :: LexParser
exclamationToken = tokenPrim show updatePos (simpleGetToken Exclamation)

andToken :: LexParser
andToken = tokenPrim show updatePos (simpleGetToken And)

orToken :: LexParser
orToken = tokenPrim show updatePos (simpleGetToken Or)

candToken :: LexParser
candToken = tokenPrim show updatePos (simpleGetToken And_cir)

corToken :: LexParser
corToken = tokenPrim show updatePos (simpleGetToken Or_cir)

---------------------------------------------------------------------------------------------------
-- Relational
---------------------------------------------------------------------------------------------------

eqToken :: LexParser
eqToken = tokenPrim show updatePos (simpleGetToken Equals)

difToken :: LexParser
difToken = tokenPrim show updatePos (simpleGetToken Not_Equals)

greaterToken :: LexParser
greaterToken = tokenPrim show updatePos (simpleGetToken Greater)

greaterEqToken :: LexParser
greaterEqToken = tokenPrim show updatePos (simpleGetToken GreaterEq)

lessToken :: LexParser
lessToken = tokenPrim show updatePos (simpleGetToken Less)

lessEqToken :: LexParser
lessEqToken = tokenPrim show updatePos (simpleGetToken LessEq)

---------------------------------------------------------------------------------------------------
-- Character chains
---------------------------------------------------------------------------------------------------

boolean :: LexParser
boolean = tokenPrim show update_pos get_token where
	get_token (Token (Bool a) l c) = Just (Bool a)
	get_token _ = Nothing

natural :: LexParser
natural = tokenPrim show updatePos getToken where
	getToken (Token (Nat n) l c) = Just (Nat n)
	getToken _ = Nothing

integer :: LexParser
integer = tokenPrim show updatePos getToken where
	getToken (Token (Int n) l c) = Just (Int n)
	getToken _ = Nothing

real :: LexParser
real = tokenPrim show updatePos getToken where
	getToken (Token (Real n) l c) = Just (Real n)
	getToken _ = Nothing

boolean :: LexParser
boolean = tokenPrim show updatePos getToken where
	getToken (Token (Bool a) l c) = Just (Bool a)
	getToken _ = Nothing

cchar :: LexParser
cchar = tokenPrim show update_pos getToken where
	getToken (Token (Char s) l c) = Just (Char s)
	getToken _ = Nothing

sstring :: LexParser
sstring = tokenPrim show update_pos getToken where
	getToken (Token (String s) l c) = Just (String s)
	getToken _ = Nothing

identifier :: LexParser
identifier = tokenPrim show updatePos getToken where
	getToken (Token (Id s) l c) = Just (Id s)
	getToken _ = Nothing

---------------------------------------------------------------------------------------------------
-- Default functions
---------------------------------------------------------------------------------------------------

updatePos p (Token t l c) _ = newPos (sourceName p) l c

simpleGetToken tok (Token t l c) = 
	if tok == t then 
		Just t
	else 
		Nothing