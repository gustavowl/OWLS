module Lexer where

import Data.Functor.Identity
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import Tokens

data OWLState = OWLState [String]

type OWLParser = ParsecT [Token] OWLState Identity
type LexParser = OWLParser Token

---------------------------------------------------------------------------------------------------
-- Program key words
---------------------------------------------------------------------------------------------------

mainToken :: LexParser
mainToken = tokenPrim show update_pos (simpleGetToken Main)

funcToken :: LexParser
funcToken = tokenPrim show update_pos get_token where
	get_token (Token Func l c) = Just (Token Func l c)
	get_token _ = Nothing

procToken :: LexParser
procToken = tokenPrim show update_pos get_token where
	get_token (Token Proc l c) = Just (Token Proc l c)
	get_token _ = Nothing

---------------------------------------------------------------------------------------------------
-- Statement key words
---------------------------------------------------------------------------------------------------

ifToken :: LexParser
ifToken = tokenPrim show update_pos (simpleGetToken If)

elseToken :: LexParser
elseToken = tokenPrim show update_pos (simpleGetToken Else)

switchToken :: LexParser
switchToken = tokenPrim show update_pos (simpleGetToken Switch)

caseToken :: LexParser
caseToken = tokenPrim show update_pos (simpleGetToken Case)

---------------------------------------------------------------------------------------------------
-- Characters / symbols
---------------------------------------------------------------------------------------------------

comma :: LexParser
comma = tokenPrim show update_pos (simpleGetToken Comma)

colon :: LexParser
colon = tokenPrim show update_pos (simpleGetToken Colon)

semi :: LexParser
semi = tokenPrim show update_pos (simpleGetToken SemiColon)

lparen :: LexParser
lparen = tokenPrim show update_pos (simpleGetToken LParen)

rparen :: LexParser
rparen = tokenPrim show update_pos (simpleGetToken RParen)

parens :: OWLParser a -> OWLParser a
parens p = do
	lparen
	s <- p
	rparen
	return $ s

lbrace :: LexParser
lbrace = tokenPrim show update_pos (simpleGetToken LBrace)

rbrace :: LexParser
rbrace = tokenPrim show update_pos (simpleGetToken RBrace)

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
plusToken = tokenPrim show update_pos (simpleGetToken Plus)

minusToken :: LexParser
minusToken = tokenPrim show update_pos (simpleGetToken Minus)

timesToken :: LexParser
timesToken = tokenPrim show update_pos (simpleGetToken Times)

divideToken :: LexParser
divideToken = tokenPrim show update_pos (simpleGetToken Divide)

modulusToken :: LexParser
modulusToken = tokenPrim show update_pos (simpleGetToken Modulus)

assignToken :: LexParser
assignToken = tokenPrim show update_pos (simpleGetToken Assign)

exclamationToken :: LexParser
exclamationToken = tokenPrim show update_pos (simpleGetToken Exclamation)

andToken :: LexParser
andToken = tokenPrim show update_pos (simpleGetToken And)

orToken :: LexParser
orToken = tokenPrim show update_pos (simpleGetToken Or)

candToken :: LexParser
candToken = tokenPrim show update_pos (simpleGetToken And_cir)

corToken :: LexParser
corToken = tokenPrim show update_pos (simpleGetToken Or_cir)

---------------------------------------------------------------------------------------------------
-- Relational
---------------------------------------------------------------------------------------------------

eqToken :: LexParser
eqToken = tokenPrim show update_pos (simpleGetToken Equals)

difToken :: LexParser
difToken = tokenPrim show update_pos (simpleGetToken Not_Equals)

greaterToken :: LexParser
greaterToken = tokenPrim show update_pos (simpleGetToken Greater)

greaterEqToken :: LexParser
greaterEqToken = tokenPrim show update_pos (simpleGetToken GreaterEq)

lessToken :: LexParser
lessToken = tokenPrim show update_pos (simpleGetToken Less)

lessEqToken :: LexParser
lessEqToken = tokenPrim show update_pos (simpleGetToken LessEq)

---------------------------------------------------------------------------------------------------
-- Character chains
---------------------------------------------------------------------------------------------------

boolean :: LexParser
boolean = tokenPrim show update_pos get_token where
	get_token (Token (Bool a) l c) = Just (Token (Bool a) l c)
	get_token _ = Nothing

natural :: LexParser
natural = tokenPrim show update_pos get_token where
	get_token (Token (Nat n) l c) = Just (Token (Nat n) l c)
	get_token _ = Nothing

integer :: LexParser
integer = tokenPrim show update_pos get_token where
	get_token (Token (Int n) l c) = Just (Token (Int n) l c)
	get_token _ = Nothing

real :: LexParser
real = tokenPrim show update_pos get_token where
	get_token (Token (Real n) l c) = Just (Token (Real n) l c)
	get_token _ = Nothing

cchar :: LexParser
cchar = tokenPrim show update_pos get_token where
	get_token (Token (Char s) l c) = Just (Token (Char s) l c)
	get_token _ = Nothing

sstring :: LexParser
sstring = tokenPrim show update_pos get_token where
	get_token (Token (String s) l c) = Just (Token (String s) l c)
	get_token _ = Nothing

identifier :: LexParser
identifier = tokenPrim show update_pos get_token where
	get_token (Token (Id s) l c) = Just (Token (Id s) l c)
	get_token _ = Nothing

---------------------------------------------------------------------------------------------------
-- Default functions
---------------------------------------------------------------------------------------------------

--update_pos :: SourcePos -> Token -> Vector Token -> SourcePos
update_pos p (Token t l c) _ = newPos (sourceName p) l c

simpleGetToken toktype (Token t l c) = 
	if toktype == t then 
		Just (Token t l c) 
	else 
		Nothing