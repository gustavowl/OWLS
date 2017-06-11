module Types where

import ProgramTree

---------------------------------------------------------------------------------------------------
-- Values
---------------------------------------------------------------------------------------------------

-- Description of variable value (primitive, struct or array)
data VarValue = NumberValue Double 
	| UserValue [VarValue] 
	| ArrayValue Integer [VarValue]
	| PointerValue Key
	| CharValue Char
	| BoolValue Bool
	| FuncValue Integer [Declaration] [Statement] -- ancestral, params (tipo + nome), corpo
	| ProcValue Integer [Declaration] [Statement]
	deriving (Eq,Show)

-- Name + scope ID
type Key = (String, Integer)

---------------------------------------------------------------------------------------------------
-- Type Convertion
---------------------------------------------------------------------------------------------------

getNumberType :: VarType -> IO String
getNumberType (AtomicType "nat") = do return "nat"
getNumberType (AtomicType "int") = do return "int"
getNumberType (AtomicType "real") = do return "real"
getNumberType a = do fail $ show a ++ " is not a number."

getNumberValue :: VarValue -> IO Double
getNumberValue (NumberValue n) = do return n
getNumberValue a = do fail $ show a ++ " is not a number."

getBoolValue :: VarValue -> IO Bool
getBoolValue (BoolValue v) = do return v
getBoolValue a = do fail $ show a ++ " is not a boolean."

getPointerValue :: VarValue -> IO Key
getPointerValue (PointerValue key) = do return key
getPointerValue a = do fail $ show a ++ " is not a pointer."

getArrayType :: VarType -> IO VarType
getArrayType (ArrayType t) = do return t
getArrayType a = do fail $ show a ++ " is not an array."

getArrayValue :: VarValue -> IO (Integer, [VarValue])
getArrayValue (ArrayValue l v) = do return (l, v)
getArrayValue a = do fail $ show a ++ " is not an array."

-- ExpectedType, ActualType, Final Type
convertType :: VarType -> VarType -> IO VarType
convertType (AtomicType "int") (AtomicType "nat") = do return $ AtomicType "int"
convertType (AtomicType "real") (AtomicType "nat") = do return $ AtomicType "real"
convertType (AtomicType "real") (AtomicType "int") = do return $ AtomicType "real"
convertType a b = do 
	if a == b then
		return a
	else
		fail $ "Could not convert " ++ show b ++ " to " ++ show a ++ "."

-- ExpectedType, ActualType
canConvertType :: VarType -> VarType -> Bool
canConvertType (AtomicType "int") (AtomicType "nat") = True
canConvertType (AtomicType "real") (AtomicType "nat") = True
canConvertType (AtomicType "real") (AtomicType "int") = True
canConvertType a b = a == b

---------------------------------------------------------------------------------------------------
-- Check Errors Functions
---------------------------------------------------------------------------------------------------

errorType :: VarType -> String
errorType (AtomicType "nat") = "nat"
errorType (AtomicType "int") = "int"
errorType (AtomicType "real") = "real"
errorType (AtomicType "char") = "char"