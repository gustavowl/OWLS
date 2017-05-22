module Lexer where

import Data.Functor.Identity
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import Tokens
import State

type TokenParser = OWLParser TokenSymbol
type IDParser = OWLParser String
type NumParser = OWLParser Double
type BoolParser = OWLParser Bool

---------------------------------------------------------------------------------------------------
-- Program key words
---------------------------------------------------------------------------------------------------

mainToken :: TokenParser
mainToken = tokenPrim show updatePos (simpleGetToken Main)

funcToken :: TokenParser
funcToken = tokenPrim show updatePos (simpleGetToken Func)

procToken :: TokenParser
procToken = tokenPrim show updatePos (simpleGetToken Proc)

---------------------------------------------------------------------------------------------------
-- Statement key words
---------------------------------------------------------------------------------------------------

ifToken :: TokenParser
ifToken = tokenPrim show updatePos (simpleGetToken If)

elseToken :: TokenParser
elseToken = tokenPrim show updatePos (simpleGetToken Else)

switchToken :: TokenParser
switchToken = tokenPrim show updatePos (simpleGetToken Switch)

caseToken :: TokenParser
caseToken = tokenPrim show updatePos (simpleGetToken Case)

returnToken :: TokenParser
returnToken = tokenPrim show updatePos (simpleGetToken Return)

---------------------------------------------------------------------------------------------------
-- Characters / symbols
---------------------------------------------------------------------------------------------------

comma :: TokenParser
comma = tokenPrim show updatePos (simpleGetToken Comma)

colon :: TokenParser
colon = tokenPrim show updatePos (simpleGetToken Colon)

semi :: TokenParser
semi = tokenPrim show updatePos (simpleGetToken SemiColon)

lparen :: TokenParser
lparen = tokenPrim show updatePos (simpleGetToken LParen)

rparen :: TokenParser
rparen = tokenPrim show updatePos (simpleGetToken RParen)

parens :: OWLParser a -> OWLParser a
parens p = do
	lparen
	s <- p
	rparen
	return $ s

lbrace :: TokenParser
lbrace = tokenPrim show updatePos (simpleGetToken LBrace)

rbrace :: TokenParser
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

plusToken :: TokenParser
plusToken = tokenPrim show updatePos (simpleGetToken Plus)

minusToken :: TokenParser
minusToken = tokenPrim show updatePos (simpleGetToken Minus)

timesToken :: TokenParser
timesToken = tokenPrim show updatePos (simpleGetToken Times)

divideToken :: TokenParser
divideToken = tokenPrim show updatePos (simpleGetToken Divide)

modulusToken :: TokenParser
modulusToken = tokenPrim show updatePos (simpleGetToken Modulus)

assignToken :: TokenParser
assignToken = tokenPrim show updatePos (simpleGetToken Assign)

exclamationToken :: TokenParser
exclamationToken = tokenPrim show updatePos (simpleGetToken Exclamation)

andToken :: TokenParser
andToken = tokenPrim show updatePos (simpleGetToken And)

orToken :: TokenParser
orToken = tokenPrim show updatePos (simpleGetToken Or)

candToken :: TokenParser
candToken = tokenPrim show updatePos (simpleGetToken And_cir)

corToken :: TokenParser
corToken = tokenPrim show updatePos (simpleGetToken Or_cir)

---------------------------------------------------------------------------------------------------
-- Relational
---------------------------------------------------------------------------------------------------

eqToken :: TokenParser
eqToken = tokenPrim show updatePos (simpleGetToken Equals)

difToken :: TokenParser
difToken = tokenPrim show updatePos (simpleGetToken Not_Equals)

greaterToken :: TokenParser
greaterToken = tokenPrim show updatePos (simpleGetToken Greater)

greaterEqToken :: TokenParser
greaterEqToken = tokenPrim show updatePos (simpleGetToken GreaterEq)

lessToken :: TokenParser
lessToken = tokenPrim show updatePos (simpleGetToken Less)

lessEqToken :: TokenParser
lessEqToken = tokenPrim show updatePos (simpleGetToken LessEq)

---------------------------------------------------------------------------------------------------
-- Character chains
---------------------------------------------------------------------------------------------------

natural :: NumParser
natural = tokenPrim show updatePos getToken where
	getToken (Token (Nat n) l c) = Just n
	getToken _ = Nothing

integer :: NumParser
integer = tokenPrim show updatePos getToken where
	getToken (Token (Int n) l c) = Just n
	getToken _ = Nothing

real :: NumParser
real = tokenPrim show updatePos getToken where
	getToken (Token (Real n) l c) = Just n
	getToken _ = Nothing

boolean :: BoolParser
boolean = tokenPrim show updatePos getToken where
	getToken (Token (Bool a) l c) = Just a
	getToken _ = Nothing

cchar :: TokenParser
cchar = tokenPrim show updatePos getToken where
	getToken (Token (Char s) l c) = Just (Char s)
	getToken _ = Nothing

sstring :: TokenParser
sstring = tokenPrim show updatePos getToken where
	getToken (Token (String s) l c) = Just (String s)
	getToken _ = Nothing

identifier :: IDParser
identifier = tokenPrim show updatePos getToken where
	getToken (Token (Id s) l c) = Just s
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