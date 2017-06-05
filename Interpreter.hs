module Interpreter where

import ProgramTree
import ProgramState

runProgram :: Program -> IO()
runProgram (decs, main) = do
	let state = addGlobalDecs decs initState
	let nextState = callFunction "main" [] (AtomicType "int") state -- TODO: pegar parâmetros do main
	print (decs, main)

addGlobalDecs :: [Declaration] -> OWLState -> OWLState
addGlobalDecs decs state = state -- TODO

callFunction :: String -> [Expr] -> VarType -> OWLState -> (OWLState, VarValue)
callFunction name params expectedType state = (state, NumberValue 0) -- TODO