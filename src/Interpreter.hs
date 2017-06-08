module Interpreter where

import System.IO
import ProgramTree
import ProgramState
import Expr --needed for getting leaf nodes values

runProgram :: Program -> IO()
runProgram (decs, main) = do
	state <- addGlobalDecs decs initState
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
	state2 <- addParameters args params state1
	(state3, v) <- runFuncBody name body ret state2
	let ret = f v where
		f (NumberValue n) = round n
		f _ = 0
	return ret
callMain _ _ _ = do return 0

callFunction :: String -> [Expr] -> VarType -> OWLState -> IO (OWLState, VarValue)
callFunction name args retType state1 = do
	let scopeID = getScopeID name state1
	let (t, v) = getVar (name, scopeID) state1
	(parentID, params, retType, body) <- getFuncInfo name t v
	-- TODO: checar tipo de retorno
	let state2 = newScope parentID state1
	state3 <- addParameters args params state2
	runFuncBody name body retType state3 >>= return

callProcedure :: String -> [Expr] -> OWLState -> IO OWLState
callProcedure name args state1 = do
	let scopeID = getScopeID name state1
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

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

data StatementResult = Finish
	| Continue
	| Return Expr
	| Break

runFuncBody :: String -> [Statement] -> VarType -> OWLState -> IO (OWLState, VarValue)
runFuncBody name [] retType state = do fail $ "Function " ++ name ++ " reached end with no return."
runFuncBody name (s:stmts) expectedType state1 = do 
	runStatement s state1 >>= f where
		f (state2, Finish) = fail $ "Function " ++ name ++ " must return a value."
		f (state2, Continue) = runFuncBody name stmts expectedType state2 >>= return
		f (state2, Return expr) = do 
			(exprType, value, state3) <- evalExpr expr state2
			-- TODO: checar tipos exprType x expectedType
			return (state3, value)
		f _ = fail "WTF"

runProcBody :: String -> [Statement] -> OWLState -> IO OWLState
runProcBody name [] state = do return state
runProcBody name (s:stmts) state1 = do 
	runStatement s state1 >>= f where
		f (state2, Return _) = fail $ "Procedure " ++ name ++ " must not return a value."
		f (state2, Continue) = runProcBody name stmts state2 >>= return
		f (state2, Finish) = do return state2
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

-- Control flow. (TODO)
runStatement (If expr ifbody elsebody) state = do
	return (state, Continue)
runStatement (While expr body) state = do
	return (state, Continue)
runStatement (For ini expr incr body) state = do
	return (state, Continue)

-- General statements. (TODO)
runStatement (ProcCall name params) state = do
	return (state, Continue)
runStatement (WriteCall expr) state1 = do 
	--print state1
	--print expr --DELETE this line
	(t, v, state2) <- evalExpr expr state1
	--print t
	--print v --DELETE this line
	--print state2
	return (state2, Continue)
runStatement (Assignment name assign) state = do
	return (state, Continue)
	
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

evalBoolExpr :: BoolNode -> OWLState -> IO (Bool, OWLState)
evalBoolExpr (BoolLit b) state = do return (b, state)
evalBoolExpr (BoolID name) state = do
	let scopeID = getScopeID name state
	let (t, v) = getVar (name, scopeID) state
	if t == AtomicType "bool" then do
		let b = f v where
			f (BoolValue b) = b
			f _ = False
		return (b, state) 
	else
		fail $ "Variable " ++ name ++ " is not a boolean."

evalBoolExpr (BoolFuncCall name args) state = do return (False, state) -- TODO

evalBoolExpr (BoolNot node) state1 = do
	(v, state2) <- evalBoolExpr node state1
	return (not v, state2)

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

evalNumExpr :: NumNode -> OWLState -> IO (Double, String, OWLState)
evalNumExpr node state = do 
	--typ: type; val: value represented/contained in node
	(val, typ, state2) <- evalNumLeaf node state
	-- TODO evaluate for other expressions (recursion)
	return (val, typ, state)

--TODO should it receive a state? For now it is not using it
--returns the value. the type of the number and the state
evalNumLeaf :: NumNode -> OWLState -> IO (Double, String, OWLState)
--Evaluates for natural numbers
evalNumLeaf (NumNat n) state = do return (n, "NumNat", state)
--Evaluates for integer numbers
evalNumLeaf (NumInt n) state = do return (n, "NumInt", state)
--Evaluates for real numbers
evalNumLeaf (NumReal n) state = do return (n, "NumReal", state)

evalNumLeaf _ state = do return (0, "só pro Haskell não frescar", state)

evalStuffExpr :: StuffNode -> OWLState -> IO (VarType, VarValue, OWLState)
evalStuffExpr node state = do
	return (AtomicType "", NumberValue 0, state) -- TODO

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------

addDec :: Declaration -> OWLState -> IO OWLState
addDec (Var name varType Nothing) state = do return $ addVarDec name varType state
addDec (Var name varType (Just e)) state1 = do 
	(t, value, state2) <- evalExpr e state1
	-- TODO: verificar tipo t com o tipo da variavel
	let state3 = addVarDec name varType state2
	let scopeID = getScopeID name state3
	return $ updateVar value (name, scopeID) state3
addDec _ state = do return state -- TODO: Function e Procedure

