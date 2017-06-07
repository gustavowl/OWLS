module FuncCall where

import State
import Lexer
import Text.ParserCombinators.Parsec
import Text.Show.Functions

type OWLFuncParser = OWLParser (VarType, VarValue)

checkType :: VarType -> VarType -> Bool
checkType (AtomicType "nat") (AtomicType "int") = True
checkType (AtomicType "int") (AtomicType "real") = True 
checkType (AtomicType "nat") (AtomicType "real") = True
checkType a b = a == b

parseFuncCall :: VarType -> OWLFuncParser
parseFuncCall t = do
	state <- getState
	name <- identifier
	let scope = getFuncScope state name
	let (_, _, params, ret, body) = getFunc state (name, scope)
	if ret == t then
		return (ret, NumberValue 0) -- TODO
	else
		fail $ "Function return type is not a " ++ show t ++ ":" ++ show ret