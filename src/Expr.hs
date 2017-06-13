module Expr where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Lexer
import ProgramTree
import qualified Tokens

---------------------------------------------------------------------------------------------------
-- General Expr
---------------------------------------------------------------------------------------------------

parseExpr :: OWLParser Expr
parseExpr = parseBoolExpr

parseExprLeaf :: OWLParser Expr
parseExprLeaf = do 
	leaf <-	(try parseFuncCall)
		<|> (try parseSizeofCall)
		<|> (try parseNewCall)
		<|> (try parseArrayCall)
		<|> (try parseArray)
		<|> (try parseString)
		<|> (try parseChar)
		<|> (try parseContent)
		<|> (try parseAddr)
		<|> (try parseID)
		<|> (parens parseExpr)
	mod <- parseLeafMods leaf
	return mod

---------------------------------------------------------------------------------------------------
-- General Leaf
---------------------------------------------------------------------------------------------------

parseID :: OWLParser Expr
parseID = do
	name <- identifier
	return $ ID name

parseFuncCall :: OWLParser Expr
parseFuncCall = do
	func <- identifier
	args <- parens $ sepBy parseExpr comma
	return $ FuncCall func args

parseReadCall :: OWLParser Expr
parseReadCall = do
	readToken
	lparen
	rparen
	return $ ReadCall

parseSizeofCall :: OWLParser Expr
parseSizeofCall = do
	sizeofToken
	expr <- parens parseExpr
	return $ SizeofCall expr

parseNewCall :: OWLParser Expr
parseNewCall = do
	newToken
	typ <- parens parseVarType
	return $ NewCall typ

parseArrayCall :: OWLParser Expr
parseArrayCall = do
	arrayToken
	args <- parens $ sepBy1 parseExpr comma
	return $ ArrayCall args 

---------------------------------------------------------------------------------------------------
-- General Struct Fields / Array Elements / Pointer Content & Addr
---------------------------------------------------------------------------------------------------

parseLeafMods :: Expr -> OWLParser Expr
parseLeafMods leaf = do
	optionMaybe ((try $ parseArrayEl leaf) 
		<|> (try $ parseField leaf)) >>= f where
			f Nothing = return leaf
			f (Just e) = parseLeafMods e >>= return

parseArrayEl :: Expr -> OWLParser Expr
parseArrayEl array = do
	index <- brackets parseExpr
	return $ ArrayEl array index

parseField :: Expr -> OWLParser Expr
parseField struct = do
	dotToken
	field <- identifier
	return $ Field struct field

---------------------------------------------------------------------------------------------------
-- General Literals
---------------------------------------------------------------------------------------------------

parseChar :: OWLParser Expr
parseChar = do
	s <- cchar
	return $ CharLit s

parseArray :: OWLParser Expr
parseArray = do
	exprs <- braces (sepBy1 parseExpr comma)
	return $ ArrayLit exprs

parseString :: OWLParser Expr
parseString = do
	s <- sstring
	return $ ArrayLit $ stringToArray s

stringToArray :: String -> [Expr]  
stringToArray [] = []  
stringToArray (x:xs) = (CharLit x) : stringToArray xs

parseAddr :: OWLParser Expr
parseAddr = do
	dollarToken
	var <- identifier <|> parens identifier
	return $ Addr var

parseContent :: OWLParser Expr
parseContent = do
	atToken
	var <- parseID <|> parens parseExpr
	return $ Content var

---------------------------------------------------------------------------------------------------
-- Boolean Expr
---------------------------------------------------------------------------------------------------

parseBoolExpr :: OWLParser Expr
parseBoolExpr = parseOrChain

parseBoolLeaf :: OWLParser Expr
parseBoolLeaf = (try parseBoolUnary)
	<|> (try parseBoolean)
	<|> (try parseExprLeaf)

---------------------------------------------------------------------------------------------------
-- Boolean Literal
---------------------------------------------------------------------------------------------------

parseBoolean :: OWLParser Expr
parseBoolean = do
	b <- boolean
	return $ BoolLit b

---------------------------------------------------------------------------------------------------
-- Boolean Unary Operators
---------------------------------------------------------------------------------------------------

parseBoolUnary :: OWLParser Expr
parseBoolUnary = parseNeg

parseNeg :: OWLParser Expr
parseNeg = do
	notToken
	n <- parseBoolLeaf
	return $ BoolNot n

---------------------------------------------------------------------------------------------------
-- Boolean Binary Operators
---------------------------------------------------------------------------------------------------

parseOrChain :: OWLParser Expr
parseOrChain = (try parseOrChainTail) <|> (try parseAndChain)

parseOrChainTail :: OWLParser Expr
parseOrChainTail = do
	e1 <- parseAndChain
	op <- orToken <|> corToken
	e2 <- parseOrChain
	if op == Tokens.Or then
		return $ BoolOr e1 e1
	else
		return $ BoolOrC e1 e2

parseAndChain :: OWLParser Expr
parseAndChain = (try parseAndChainTail) <|> (try parseRelational)

parseAndChainTail :: OWLParser Expr
parseAndChainTail = do
	e1 <- parseRelational
	op <- andToken <|> candToken
	e2 <- parseAndChain
	if op == Tokens.And then
		return $ BoolAnd e1 e1
	else
		return $ BoolAndC e1 e2

---------------------------------------------------------------------------------------------------
-- Boolean Relational Binary Operators
---------------------------------------------------------------------------------------------------

parseRelational :: OWLParser Expr
parseRelational = (try parseRelationalTail) <|> (try parseNumExpr) <|> parseBoolLeaf

parseRelationalTail :: OWLParser Expr
parseRelationalTail = do
	e1 <- parseNumExpr
	op <- greaterToken <|> greaterEqToken <|> lessToken <|> lessEqToken <|> eqToken <|> difToken
	e2 <- parseNumExpr
	if op == Tokens.Equals then
		return $ BoolEq e1 e2
	else if op == Tokens.Not_Equals then
		return $ BoolDif e1 e2
	else if op == Tokens.Greater then
		return $ BoolGt e1 e2
	else if op == Tokens.Less then
		return $ BoolLt e1 e2
	else if op == Tokens.GreaterEq then
		return $ BoolGtEq e1 e2
	else
		return $ BoolLtEq e1 e2

---------------------------------------------------------------------------------------------------
-- Numeric Expr
---------------------------------------------------------------------------------------------------

parseNumExpr :: OWLParser Expr
parseNumExpr = parseAddChain

parseNumLeaf :: OWLParser Expr
parseNumLeaf = (try parseNumUnary)
	<|> (try parseNumber)
	<|> (try parseFloorCall)
	<|> (try parseCeilCall)
	<|> (try parseReadNatCall)
	<|> (try parseReadIntCall)
	<|> (try parseReadRealCall)
	<|> (try parseExprLeaf)

---------------------------------------------------------------------------------------------------
-- Numeric Literal Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumber :: OWLParser Expr
parseNumber = parseNatural <|> parseInteger <|> parseReal

parseNatural :: OWLParser Expr
parseNatural = do
	n <- natural
	return $ NatLit n

parseInteger :: OWLParser Expr
parseInteger = do
	n <- integer
	return $ IntLit n

parseReal :: OWLParser Expr
parseReal = do
	n <- real
	return $ RealLit n

---------------------------------------------------------------------------------------------------
-- Numeric Functions
---------------------------------------------------------------------------------------------------

parseFloorCall :: OWLParser Expr
parseFloorCall = do
	floorToken
	expr <- parens parseExpr
	return $ FloorCall expr

parseCeilCall :: OWLParser Expr
parseCeilCall = do
	ceilToken
	expr <- parens parseExpr
	return $ CeilCall expr

parseReadNatCall :: OWLParser Expr
parseReadNatCall = do
	readNatToken
	lparen
	rparen
	return $ ReadNatCall

parseReadIntCall :: OWLParser Expr
parseReadIntCall = do
	readIntToken
	lparen
	rparen
	return $ ReadIntCall

parseReadRealCall :: OWLParser Expr
parseReadRealCall = do
	readRealToken
	lparen
	rparen
	return $ ReadRealCall


---------------------------------------------------------------------------------------------------
-- Numeric Unary Operators
---------------------------------------------------------------------------------------------------

parseNumUnary :: OWLParser Expr
parseNumUnary = parseMinus

parseMinus :: OWLParser Expr
parseMinus = do
	op <- minusToken
	expr <- parseNumLeaf
	return $ NumMinus expr

---------------------------------------------------------------------------------------------------
-- Numeric Binary Operators
---------------------------------------------------------------------------------------------------

parseAddChain :: OWLParser Expr
parseAddChain = do
	e1 <- parseMulChain
	e2 <- parseAddChainTail e1
	return e2

parseAddChainTail :: Expr -> OWLParser Expr
parseAddChainTail e1 = do
	optionMaybe (plusToken <|> minusToken) >>= f where
		f Nothing = do return e1
		f (Just op) = do
			e2 <- parseMulChain
			if op == Tokens.Plus then
				parseAddChainTail (NumAdd e1 e2) >>= return
			else
				parseAddChainTail (NumSub e1 e2) >>= return

parseMulChain :: OWLParser Expr
parseMulChain = do
	e1 <- parseNumLeaf
	e2 <- parseMulChainTail e1
	return e2

parseMulChainTail :: Expr -> OWLParser Expr
parseMulChainTail e1 = do
	optionMaybe (timesToken <|> divideToken <|> modulusToken) >>= f where
		f Nothing = do return e1
		f (Just op) = do
			e2 <- parseNumLeaf
			if op == Tokens.Times then
				parseMulChainTail (NumMul e1 e2) >>= return
			else if op == Tokens.Divide then
				parseMulChainTail (NumDiv e1 e2) >>= return
			else
				parseMulChainTail (NumMod e1 e2) >>= return

---------------------------------------------------------------------------------------------------
-- Var Types
---------------------------------------------------------------------------------------------------

parseVarType :: OWLParser VarType
parseVarType = (try parseAtomicType) 
	<|> (try parseArrayType) 
	<|> (try parsePointerType)
	<|> (try parseFuncType)
	<|> (try parseProcType)
	<|> (parens parseVarType)

parseAtomicType :: OWLParser VarType
parseAtomicType = do
	id <- identifier
	return $ AtomicType id

parseArrayType :: OWLParser VarType
parseArrayType = do
	t <- brackets parseVarType
	return $ ArrayType t 

parsePointerType :: OWLParser VarType
parsePointerType = do
	atToken
	t <- parseVarType
	return $ PointerType t

parseFuncType :: OWLParser VarType
parseFuncType = do
	funcToken
	params <- parseTypeList
	ret <- parens parseVarType
	return $ FuncType params ret

parseProcType :: OWLParser VarType
parseProcType = do
	procToken
	params <- parseTypeList
	return $ ProcType params

parseTypeList :: OWLParser [VarType]
parseTypeList = parens (sepBy parseVarType comma)
