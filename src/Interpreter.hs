module Interpreter where

import Data.Fixed
import System.IO
import ProgramTree
import ProgramState
import Expr --needed for getting leaf nodes values

runProgram :: Program -> IO()
runProgram (types, decs, main) = do
	state <- addGlobalDecs decs (initState types)
	i <- callMain getMainArgs main state
	print i

addGlobalDecs :: [Declaration] -> OWLState -> IO OWLState
addGlobalDecs [] state = do return state
addGlobalDecs (h:decs) state = do
	state2 <- (addDec h state)
	state3 <- addGlobalDecs decs state2
	return state3

getMainArgs :: [Expr]
getMainArgs = []
-- TODO: pegar parâmetros reais passados no terminal (não é prioridade)

callMain :: [Expr] -> Declaration -> OWLState -> IO Integer
callMain args (Function name params ret body) state1 = do
	let state2 = newScope 0 state1
	state3 <- addParameters args params state2
	(state4, v) <- runFuncBody name body ret state3
	print state4
	let ret = f v where
		f (NumberValue n) = round n
		f _ = 0
	return ret
callMain _ _ _ = do return 0

callFunction :: Key -> [Expr] -> OWLState -> IO (OWLState, VarType, VarValue)
callFunction (name, scopeID) args state1 = do
	let (t, v) = getVar (name, scopeID) state1
	(parentID, params, retType, body) <- getFuncInfo name t v
	let state2 = newScope parentID state1
	state3 <- addParameters args params state2
	(state4, value) <- runFuncBody name body retType state3
	return (state4, retType, value)

callProcedure :: Key -> [Expr] -> OWLState -> IO OWLState
callProcedure (name, scopeID) args state1 = do
	let (t, v) = getVar (name, scopeID) state1
	(parentID, params, body) <- getProcInfo name t v
	let state2 = newScope parentID state1
	state3 <- addParameters args params state2
	runProcBody name body state3 >>= return

addParameters :: [Expr] -> [Declaration] -> OWLState -> IO OWLState
addParameters [] [] state = do return state
addParameters (a:args) ((Var name t1 expr):params) state1 = do 
	(t2, v, state2) <- evalExpr a state1
	convertType t1 t2
	let state3 = addVarDec name t1 state2
	let scopeID = getScopeID name state3
	let state4 = updateVar v (name, scopeID) state3
	addParameters args params state4
addParameters (a:args) ((Function name p ret body):params) state1 = do 
	let t1 = FuncType (extractParamTypes p) ret
	(t2, v, state2) <- evalExpr a state1
	convertType t1 t2
	let state3 = addVarDec name t1 state2
	let scopeID = getScopeID name state3
	let state4 = updateVar v (name, scopeID) state3
	addParameters args params state4
addParameters (a:args) ((Procedure name p body):params) state1 = do 
	let t1 = ProcType (extractParamTypes p)
	(t2, v, state2) <- evalExpr a state1
	convertType t1 t2
	let state3 = addVarDec name t1 state2
	let scopeID = getScopeID name state3
	let state4 = updateVar v (name, scopeID) state3
	addParameters args params state4

addParameters r f s = fail "WTF"

getFuncInfo :: String -> VarType -> VarValue -> IO (Integer, [Declaration], VarType, [Statement])
getFuncInfo name (FuncType _ retType) (FuncValue parentID params body) = do
	return (parentID, params, retType, body)
getFuncInfo name _ _ = do fail $ name ++ " is not a function."

getProcInfo :: String -> VarType -> VarValue -> IO (Integer, [Declaration], [Statement])
getProcInfo name (ProcType _) (ProcValue parentID params body) = do
	return (parentID, params, body)
getProcInfo name _ _ = do fail $ name ++ " is not a procedure."

getFuncRetType :: Key -> OWLState -> IO VarType
getFuncRetType (name, scopeID) state = do
	let (t, v) = getVar (name, scopeID) state
	(parentID, params, retType, body) <- getFuncInfo name t v
	return retType

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

data StatementResult = Finish
	| Continue
	| Return Expr
	| BreakCall

runFuncBody :: String -> [Statement] -> VarType -> OWLState -> IO (OWLState, VarValue)
runFuncBody name [] retType state = do fail $ "Function " ++ name ++ " reached end with no return."
runFuncBody name (s:stmts) expectedType state1 = do 
	runStatement s state1 >>= f where
		f (state2, Finish) = fail $ "Function " ++ name ++ " must return a value."
		f (state2, Continue) = runFuncBody name stmts expectedType state2 >>= return
		f (state2, Return expr) = do 
			(exprType, value, state3) <- evalExpr expr state2
			convertType expectedType exprType
			return (state3, value)
		f (state2, BreakCall) = fail "Break is not breaking anything."

-- Verificar melhor posicao para adicionar
errorType :: VarType -> String
errorType (AtomicType "nat") = "nat"
errorType (AtomicType "int") = "int"
errorType (AtomicType "real") = "real"
errorType (AtomicType "char") = "char"

runProcBody :: String -> [Statement] -> OWLState -> IO OWLState
runProcBody name [] state = do return state
runProcBody name (s:stmts) state1 = do 
	runStatement s state1 >>= f where
		f (state2, Return _) = fail $ "Procedure " ++ name ++ " must not return a value."
		f (state2, Continue) = runProcBody name stmts state2 >>= return
		f (state2, Finish) = do return state2
		f (state2, BreakCall) = fail "Break is not breaking anything."

runIfElseBody :: [Statement] -> OWLState -> IO (OWLState, StatementResult)
runIfElseBody [] state = do return (state, Continue)
runIfElseBody (s:stmts) state = do 
	runStatement s state >>= f where
		f (state1, Return expr) = do return (state1, Return expr)
		f (state1, Continue) = (runIfElseBody stmts state1)
		f _ = fail "WTF"

-- Statement pra interpretar -> valor esperado para o return (se houver) -> estado atual -> novo estado
runStatement :: Statement -> OWLState -> IO (OWLState, StatementResult)
-- Declarations.
runStatement (VarDec dec) state1 = do 
	state2 <- addDec dec state1
	return (state2, Continue)
runStatement (FuncDec dec) state1 = do 
	state2 <- addDec dec state1
	return (state2, Continue)
runStatement (ProcDec dec) state1 = do 
	state2 <- addDec dec state1
	return (state2, Continue)

-- Return statements.
runStatement (ProcRet) state = do
	return (state, Finish)
runStatement (FuncRet expr) state = do
	return (state, Return expr) 

runStatement (If expr ifbody elsebody) state1 = do
	(varType, varValue, state2) <- evalExpr expr state1
	convertType (AtomicType "bool") varType
	if varValue == BoolValue True then
		runIfElseBody ifbody state2
	else
		runIfElseBody elsebody state2

runStatement (While expr body) state = do
	return (state, Continue)
runStatement (For ini expr incr body) state = do
	return (state, Continue)

-- General statements. (TODO)
runStatement (ProcCall name args) state1 = do
	let scopeID = getScopeID name state1
	state2 <- callProcedure (name, scopeID) args state1
	return (state2, Continue)

runStatement (WriteCall expr) state1 = do 
	(t, v, state2) <- evalExpr expr state1
	printValue v -- Ver printValue (note que falta definir como imprimir alguns tipos)
	return (state2, Continue)
runStatement (Assignment name assign) state1 = do -- minha versão está dando erro de tipo
	let scope = getScopeID name state1
	let (varTypeAssign, _) = getVar (name, scope) state1
	(varType, value, state2) <- evalExpr assign state1
	-- TODO verificar tipo varType x varTypeAssign 
	let state3 = updateVar value (name, scope) state2
	return (state3, Continue)

printValue :: VarValue -> IO()
printValue (BoolValue b) = do
	print b
printValue (CharValue c) = do
	print c
printValue (NumberValue d) = do
	print d
printValue v = do
	print v

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------

addDec :: Declaration -> OWLState -> IO OWLState
addDec (Var name varType Nothing) state = do return $ addVarDec name varType state
addDec (Var name varType (Just e)) state1 = do 
	(actualType, value, state2) <- evalExpr e state1
	convertType varType actualType
	let state3 = addVarDec name varType state2
	let scopeID = getScopeID name state3
	return $ updateVar value (name, scopeID) state3

addDec (Function name params ret body) state = do
	return $ addFuncDec name params ret body state
addDec (Procedure name params body) state = do
	return $ addProcDec name params body state

---------------------------------------------------------------------------------------------------
-- Evaluate Expression
---------------------------------------------------------------------------------------------------

evalBoolExpr :: Expr -> OWLState -> IO (Bool, OWLState)
evalBoolExpr expr state1 = do
	(t, v, state2) <- evalExpr expr state1
	convertType (AtomicType "bool") t
	b <- getBoolValue v
	return (b, state2) 

boolToExpr :: Bool -> OWLState -> (VarType, VarValue, OWLState)
boolToExpr b state = (AtomicType "bool", BoolValue b, state)

evalNumExpr :: Expr -> OWLState -> IO (String, Double, OWLState)
evalNumExpr expr state1 = do
	(t, v, state2) <- evalExpr expr state1
	numt <- getNumberType t
	numv <- getNumberValue v
	return (numt, numv, state2)

numToExpr :: String -> Double -> OWLState -> (VarType, VarValue, OWLState)
numToExpr typ val state = (AtomicType typ, NumberValue val, state)

convertArrayCharToExpr :: [Char] -> VarValue
convertArrayCharToExpr arrayChar = ArrayValue [] -- TODO

-- Calcula o valor da expressão
evalExpr :: Expr -> OWLState -> IO (VarType, VarValue, OWLState)

---------------------------------------------------------------------------------------------------
-- Evaluate Leaves
---------------------------------------------------------------------------------------------------

-- Variable.
evalExpr (ID name) state = do
	let scopeID = getScopeID name state
	let (t, v) = getVar (name, scopeID) state
	return (t, v, state)

-- Funcion call.
evalExpr (FuncCall name args) state1 = do
	let scopeID = getScopeID name state1
	(state2, t, v) <- callFunction (name, scopeID) args state1
	return (t, v, state2)

-- Read call.
evalExpr (ReadCall) state = do
	line <- getLine
	let expr = convertArrayCharToExpr line
	let size = NatLit $ (fromIntegral (length line)) + 0.0
	return (ArrayType (AtomicType "char") size, expr, state)

-- TODO: array element, pointer, field

---------------------------------------------------------------------------------------------------
-- Evaluate Literals
---------------------------------------------------------------------------------------------------

evalExpr (BoolLit b) state = do
	return (AtomicType "bool", BoolValue b, state)

evalExpr (NatLit n) state = do
	return (AtomicType "nat", NumberValue n, state)

evalExpr (IntLit n) state = do
	return (AtomicType "int", NumberValue n, state)

evalExpr (RealLit n) state = do
	return (AtomicType "real", NumberValue n, state)

evalExpr (CharLit c) state = do
	return (AtomicType "char", CharValue c, state)

evalExpr (ArrayLit n) state = do
	return (AtomicType "nat", NumberValue 0, state) -- TODO: calcular cada elemento da array

---------------------------------------------------------------------------------------------------
-- Evaluate Boolean Operators
---------------------------------------------------------------------------------------------------

-- Boolean NOT.
evalExpr (BoolNot expr) state1 = do
	(b, state2) <- evalBoolExpr expr state1
	return $ boolToExpr (not b) state2 

-- Boolean AND.
evalExpr (BoolOr expr1 expr2) state1 = do
	(b1, state2) <- evalBoolExpr expr1 state1
	(b2, state3) <- evalBoolExpr expr2 state2
	return $ boolToExpr (b1 || b2) state3

-- Boolean OR.
evalExpr (BoolAnd expr1 expr2) state1 = do
	(b1, state2) <- evalBoolExpr expr1 state1
	(b2, state3) <- evalBoolExpr expr2 state2
	return $ boolToExpr (b1 && b2) state3

evalExpr (BoolOrC expr1 expr2) state1 = do
	(b1, state2) <- evalBoolExpr expr1 state1
	if not b1 then do
		(b2, state3) <- evalBoolExpr expr2 state2
		return $ boolToExpr b2 state3
	else do
		return $ boolToExpr True state2

evalExpr (BoolAndC expr1 expr2) state1 = do
	(b1, state2) <- evalBoolExpr expr1 state1
	if b1 then do
		(b2, state3) <- evalBoolExpr expr2 state2
		return $ boolToExpr b2 state3
	else do
		return $ boolToExpr False state2

---------------------------------------------------------------------------------------------------
-- Evaluate Relationals
---------------------------------------------------------------------------------------------------

-- Equals.
evalExpr (BoolEq e1 e2) state1 = do
	(t1, v1, state2) <- evalExpr e1 state1
	(t2, v2, state3) <- evalExpr e2 state2
	if v1 == v2 then
		return $ boolToExpr True state3
	else
		return $ boolToExpr False state3

-- Differs.
evalExpr (BoolDif e1 e2) state1 = do
	(b, state2) <- evalBoolExpr (BoolEq e1 e2) state1
	return $ boolToExpr (not b) state2

-- Greater than.
evalExpr (BoolGt e1 e2) state1 = do
	(t1, v1, state2) <- evalNumExpr e1 state1
	(t2, v2, state3) <- evalNumExpr e2 state2
	if v1 > v2 then
		return $ boolToExpr True state3
	else
		return $ boolToExpr False state3

-- Greater than or equal to.
evalExpr (BoolGtEq e1 e2) state1 = do
	(t1, v1, state2) <- evalNumExpr e1 state1
	(t2, v2, state3) <- evalNumExpr e2 state2
	if v1 >= v2 then
		return $ boolToExpr True state3
	else
		return $ boolToExpr False state3

-- Less than.
evalExpr (BoolLt e1 e2) state = evalExpr (BoolGt e2 e1) state

-- Less than or equal to.
evalExpr (BoolLtEq e1 e2) state = evalExpr (BoolGtEq e2 e1) state

---------------------------------------------------------------------------------------------------
-- Evaluate Numeric Operators
---------------------------------------------------------------------------------------------------

-- Numeric negation.
evalExpr (NumMinus expr) state1 = do
	(typ, val, state2) <- evalNumExpr expr state1
	if typ == "real" then
		return $ numToExpr "real" (-val) state2 
	else
		return $ numToExpr "int" (-val) state2

-- Numeric addition.
evalExpr (NumAdd expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	if typ1 == "real" || typ2 == "real" then
		return $ numToExpr "real" (val1 + val2) state3 
	else if typ1 == "int" || typ2 == "int" then
		return $ numToExpr "int" (val1 + val2) state3
	else
		return $ numToExpr "nat" (val1 + val2) state3

-- Numeric subtraction.
evalExpr (NumSub expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	if typ1 == "real" || typ2 == "real" then
		return $ numToExpr "real" (val1 - val2) state3
	else 
		return $ numToExpr "int" (val1 - val2) state3

-- Numeric multiplication.
evalExpr (NumMul expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	if typ1 == "real" || typ2 == "real" then
		return $ numToExpr "real" (val1 * val2) state3
	else if typ1 == "int" || typ2 == "int" then
		return $ numToExpr "int" (val1 * val2) state3
	else
		return $ numToExpr "nat" (val1 * val2) state3

-- Numeric division.
evalExpr (NumDiv expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	return $ numToExpr "real" (val1 / val2) state3

-- Numeric modulus.
evalExpr (NumMod expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	if typ1 == "real" || typ2 == "real" then
		return $ numToExpr "real" (mod' val1 val2) state3
	else if typ1 == "int" || typ2 == "int" then
		return $ numToExpr "int" (mod' val1 val2) state3
	else
		return $ numToExpr "nat" (mod' val1 val2) state3