module ProgramState where

type OWLState = (OWLScope, [OWLScope])

type OWLScope = (Integer, Integer, [TableEntry])



type TableEntry = ()

---------------------------------------------------------------------------------------------------
-- Values
---------------------------------------------------------------------------------------------------
-- Description of variable value (primitive, struct or array)
data VarValue = NumberValue Double 
	| UserValue [VarValue] 
	| ArrayValue [VarValue]
	| PointerValue Key
	| CharValue Char
	| BoolValue Bool
	| FuncValue Key
	| ProcValue Key
	deriving (Eq,Show)

-- Name + scope ID
type Key = (String, Integer)

---------------------------------------------------------------------------------------------------
-- Initial Values
---------------------------------------------------------------------------------------------------

initState :: OWLState
initState = ([], [], [], [])

getInitValue :: VarType -> VarValue
getInitValue (AtomicType "nat") = NumberValue 0
getInitValue (AtomicType "int") = NumberValue 0
getInitValue (AtomicType "real") = NumberValue 0
getInitValue (AtomicType "char") = NumberValue 0
getInitValue (AtomicType "bool") = BoolValue False
getInitValue (AtomicType _) = UserValue [] -- TODO: initialize each field
getInitValue (PointerType _) = PointerValue ("", 0)
getInitValue (ArrayType _ n) = ArrayValue [] -- TODO: initialize each element
getInitValue (FuncType _) = FuncValue ("", -1)
getInitValue (ProcType _) = ProcValue ("", -1)