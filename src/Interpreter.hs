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

callFunction :: Key -> [Expr] -> OWLState -> IO (OWLState, VarValue)
callFunction (name, scopeID) args state1 = do
	let (t, v) = getVar (name, scopeID) state1
	(parentID, params, retType, body) <- getFuncInfo name t v
	let state2 = newScope parentID state1
	state3 <- addParameters args params state2
	runFuncBody name body retType state3 >>= return

callProcedure :: Key -> [Expr] -> OWLState -> IO OWLState
callProcedure (name, scopeID) args state1 = do
	let (t, v) = getVar (name, scopeID) state1
	(parentID, params, body) <- getProcInfo name t v
	let state2 = newScope parentID state1
	state3 <- addParameters args params state2
	runProcBody name body state3 >>= return

addParameters :: [Expr] -> [Declaration] -> OWLState -> IO OWLState
addParameters args params state = do return state 
-- TODO: adicionar variáveis para cada parâmetro e checar os tipos de cada

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
-- Valuate Expression
---------------------------------------------------------------------------------------------------

-- Calcula o valor da expressão
evalExpr :: Expr -> OWLState -> IO (VarType, VarValue, OWLState)
evalExpr (BoolExpr node) state1 = do
	(b, state2) <- evalBoolExpr node state1
	return (AtomicType "bool", BoolValue b, state2)

evalExpr (NumExpr node) state1 = do
	(n, s, state2) <- evalNumExpr node state1
	return (AtomicType s, NumberValue n, state2)

evalExpr (StuffExpr node) state = evalStuffExpr node state

---------------------------------------------------------------------------------------------------
-- Evaluate Boolean Expression
---------------------------------------------------------------------------------------------------

evalBoolExpr :: BoolNode -> OWLState -> IO (Bool, OWLState)

-- Boolean literal.
evalBoolExpr (BoolLit b) state = do return (b, state)

-- Boolean variable.
evalBoolExpr (BoolID name) state = do
	let scopeID = getScopeID name state
	let (t, v) = getVar (name, scopeID) state
	convertType (AtomicType "bool") t
	b <- getBoolValue v
	return (b, state)

-- Function with boolean return.
evalBoolExpr (BoolFuncCall name args) state1 = do 
	let scopeID = getScopeID name state1
	retType <- getFuncRetType (name, scopeID) state1
	convertType (AtomicType "bool") retType
	(state2, value) <- callFunction (name, scopeID) args state1
	b <- getBoolValue value
	return (b, state2)

-- Boolean not.
evalBoolExpr (BoolNot node) state1 = do
	(v, state2) <- evalBoolExpr node state1
	return (not v, state2)

-- Boolean and.
evalBoolExpr (BoolAnd node1 node2) state1 = do
	(v1, state2) <- evalBoolExpr node1 state1
	(v2, state3) <- evalBoolExpr node2 state2
	return (v1 && v2, state3)

evalBoolExpr (BoolOr node1 node2) state1 = do
	(v1, state2) <- evalBoolExpr node1 state1
	(v2, state3) <- evalBoolExpr node2 state2
	return (v1 || v2, state3)

evalBoolExpr (BoolAndC node1 node2) state1 = do
	(v1, state2) <- evalBoolExpr node1 state1
	if v1 then do
		(v2, state3) <- evalBoolExpr node2 state2
		return (v2, state3)
	else do
		return (False, state2)

evalBoolExpr (BoolOrC node1 node2) state1 = do
	(v1, state2) <- evalBoolExpr node1 state1
	if not v1 then do
		(v2, state3) <- evalBoolExpr node2 state2
		return (v2, state3)
	else do
		return (True, state2)

evalBoolExpr (BoolEq e1 e2) state1 = do
	(t1, v1, state2) <- evalExpr e1 state1
	(t2, v2, state3) <- evalExpr e2 state2
	if t1 == t2 && v1 == v2 then
		return (True, state3)
	else
		return (False, state3)

evalBoolExpr (BoolDif e1 e2) state1 = do
	(b, state2) <- evalBoolExpr (BoolEq e1 e2) state1
	return (not b, state2)

evalBoolExpr (BoolGt e1 e2) state1 = do
	(t1, NumberValue v1, state2) <- evalExpr e1 state1
	(t2, NumberValue v2, state3) <- evalExpr e2 state2
	if v1 > v2 then
		return (True, state3)
	else
		return (False, state3)

evalBoolExpr (BoolGtEq e1 e2) state1 = do
	(t1, NumberValue v1, state2) <- evalExpr e1 state1
	(t2, NumberValue v2, state3) <- evalExpr e2 state2
	if v1 >= v2 then
		return (True, state3)
	else
		return (False, state3)

evalBoolExpr (BoolLt e1 e2) state = evalBoolExpr (BoolGt e2 e1) state
evalBoolExpr (BoolLtEq e1 e2) state = evalBoolExpr (BoolGtEq e2 e1) state

---------------------------------------------------------------------------------------------------
-- Valuate Numeric Expression
---------------------------------------------------------------------------------------------------

evalNumExpr :: NumNode -> OWLState -> IO (Double, String, OWLState)

-- Numeric literals.
evalNumExpr (NumNat n) state = do return (n, "nat", state)
evalNumExpr (NumInt n) state = do return (n, "int", state)
evalNumExpr (NumReal n) state = do return (n, "real", state)

-- Numeric variables.
evalNumExpr (NumID name) state = do
	let scope = getScopeID name state
	let (varType, value) = getVar (name, scope) state
	-- print state -- (DEBUG)
	v <- getNumberValue value
	t <- getNumberType varType
	return (v, t, state)

-- Function with numeric return.
evalNumExpr (NumFuncCall name args) state1 = do
	let scopeID = getScopeID name state1
	retType <- getFuncRetType (name, scopeID) state1
	t <- getNumberType retType
	(state2, value) <- callFunction (name, scopeID) args state1
	n <- getNumberValue value
	return (n, t, state2)

-- Array with numeric elements.
evalNumExpr (NumEl array num) state = do return (0, "nat", state) -- TODO

-- Numeric negative.
evalNumExpr (NumMinus node) state1 = do
	(val, typ, state2) <- evalNumExpr node state1
	if typ == "real" then
		return (-val, "real", state2) 
	else
		return (-val, "int", state2)

-- Numeric addition.
evalNumExpr (NumAdd node1 node2) state1 = do
	(val1, typ1, state2) <- evalNumExpr node1 state1
	(val2, typ2, state3) <- evalNumExpr node2 state2
	if typ1 == "real" || typ2 == "real" then
		return (val1 + val2, "real", state3) 
	else if typ1 == "int" || typ2 == "int" then
		return (val1 + val2, "int", state3)
	else
		return (val1 + val2, "nat", state3)

-- Numeric subtraction.
evalNumExpr (NumSub node1 node2) state1 = do
	(val1, typ1, state2) <- evalNumExpr node1 state1
	(val2, typ2, state3) <- evalNumExpr node2 state2
	if typ1 == "real" || typ2 == "real" then
		return (val1 - val2, "real", state3) 
	else 
		return (val1 - val2, "int", state3)

-- Numeric multiplication.
evalNumExpr (NumMul node1 node2) state1 = do
	(val1, typ1, state2) <- evalNumExpr node1 state1
	(val2, typ2, state3) <- evalNumExpr node2 state2
	if typ1 == "real" || typ2 == "real" then
		return (val1 * val2, "real", state3) 
	else if typ1 == "int" || typ2 == "int" then
		return (val1 * val2, "int", state3)
	else
		return (val1 * val2, "nat", state3)

-- Numeric division.
evalNumExpr (NumDiv node1 node2) state1 = do
	(val1, typ1, state2) <- evalNumExpr node1 state1
	(val2, typ2, state3) <- evalNumExpr node2 state2
	return (val1 / val2, "real", state3)

-- Numeric modulus.
evalNumExpr (NumMod node1 node2) state1 = do
	(val1, typ1, state2) <- evalNumExpr node1 state1
	(val2, typ2, state3) <- evalNumExpr node2 state2
	if typ1 == "real" || typ2 == "real" then
		return (mod' val1 val2, "real", state3) 
	else if typ1 == "int" || typ2 == "int" then
		return (mod' val1 val2, "int", state3)
	else
		return (mod' val1 val2, "nat", state3)

---------------------------------------------------------------------------------------------------
-- Evaluate Stuff Expression
---------------------------------------------------------------------------------------------------

evalStuffExpr :: StuffNode -> OWLState -> IO (VarType, VarValue, OWLState)
evalStuffExpr node state = do
	return (AtomicType "", NumberValue 0, state) -- TODO