module TypeExpr where

import Text.ParserCombinators.Parsec
import ProgramTree
import Lexer
import Expr

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