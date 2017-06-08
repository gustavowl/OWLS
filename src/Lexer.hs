module Lexer where

import Data.Functor.Identity
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import qualified ProgramTree as P 
import Tokens

type TokenParser = P.OWLParser TokenSymbol
type IDParser = P.OWLParser String
type NumParser = P.OWLParser Double
type BoolParser = P.OWLParser Bool
type CharParser = P.OWLParser Char
type StringParser = P.OWLParser String

---------------------------------------------------------------------------------------------------
-- Program key words
---------------------------------------------------------------------------------------------------

mainToken :: TokenParser
mainToken = tokenPrim show updatePos (simpleGetToken Main)

funcToken :: TokenParser
funcToken = tokenPrim show updatePos (simpleGetToken Func)

procToken :: TokenParser
procToken = tokenPrim show updatePos (simpleGetToken Proc)

readToken :: TokenParser
readToken = tokenPrim show updatePos (simpleGetToken Read)

writeToken :: TokenParser
writeToken = tokenPrim show updatePos (simpleGetToken Write)

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

whileToken :: TokenParser
whileToken = tokenPrim show updatePos (simpleGetToken While)

forToken :: TokenParser
forToken = tokenPrim show updatePos (simpleGetToken For)

returnToken :: TokenParser
returnToken = tokenPrim show updatePos (simpleGetToken Return)

breakToken :: TokenParser
breakToken = tokenPrim show updatePos (simpleGetToken Break)

constToken :: TokenParser
constToken = tokenPrim show updatePos (simpleGetToken Const)

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

parens :: P.OWLParser a -> P.OWLParser a
parens p = do
	lparen
	s <- p
	rparen
	return $ s

lbrace :: TokenParser
lbrace = tokenPrim show updatePos (simpleGetToken LBrace)

rbrace :: TokenParser
rbrace = tokenPrim show updatePos (simpleGetToken RBrace)

braces :: P.OWLParser a -> P.OWLParser a
braces p = do
	lbrace
	s <- p
	rbrace
	return $ s

lbracket :: TokenParser
lbracket = tokenPrim show updatePos (simpleGetToken LBracket)

rbracket :: TokenParser
rbracket = tokenPrim show updatePos (simpleGetToken RBracket)

brackets :: P.OWLParser a -> P.OWLParser a
brackets p = do
	lbracket
	s <- p
	rbracket
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

notToken :: TokenParser
notToken = tokenPrim show updatePos (simpleGetToken Exclamation)

andToken :: TokenParser
andToken = tokenPrim show updatePos (simpleGetToken And)

orToken :: TokenParser
orToken = tokenPrim show updatePos (simpleGetToken Or)

candToken :: TokenParser
candToken = tokenPrim show updatePos (simpleGetToken And_cir)

corToken :: TokenParser
corToken = tokenPrim show updatePos (simpleGetToken Or_cir)

atToken :: TokenParser
atToken = tokenPrim show updatePos (simpleGetToken At)

dollarToken :: TokenParser
dollarToken = tokenPrim show updatePos (simpleGetToken Dollar)

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

cchar :: CharParser
cchar = tokenPrim show updatePos getToken where
	getToken (Token (CChar s) l c) = Just s
	getToken _ = Nothing

sstring :: StringParser
sstring = tokenPrim show updatePos getToken where
	getToken (Token (SString s) l c) = Just s
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