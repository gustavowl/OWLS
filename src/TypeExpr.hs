module TypeExpr where

import Text.ParserCombinators.Parsec
import ProgramTree
import Lexer
import Expr

parseVarType :: OWLParser VarType
parseVarType = try parsePointerType 
	<|> try parseArrayType 
	<|> try parseFuncType 
	<|> try parseProcType 
	<|> parseAtomicType

parseTypeLeaf :: OWLParser VarType
parseTypeLeaf = parseAtomicType <|> (parens parseVarType)

parseAtomicType :: OWLParser VarType
parseAtomicType = do
	id <- identifier
	return $ AtomicType id

parsePointerType :: OWLParser VarType
parsePointerType = do
	atToken
	t <- parseTypeLeaf
	return $ PointerType t

parseArrayType :: OWLParser VarType
parseArrayType = do
	t <- parseTypeLeaf
	size <- brackets parseExpr
	return $ ArrayType t size

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