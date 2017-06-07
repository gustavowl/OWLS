module Interpreter where

import ProgramTree
import ProgramState

runProgram :: Program -> IO()
runProgram (decs, main) = do
	let state = addGlobalDecs decs initState
	let nextState = callFunction "main" getMainParams (AtomicType "int") state 
	print (decs, main)

getMainParams :: [Expr]
getMainParams = [] 
-- TODO: pegar parâmetros reais passados no terminal (não é prioridade)

callFunction :: String -> [Expr] -> VarType -> OWLState -> (OWLState, VarValue)
callFunction name params expectedType state = (state, NumberValue 0)
	-- TODO:
	-- 1) Pegar informações da função na tabela de símbolos a partir do nome dela
	-- 2) Pegar o parentID dessa função
	-- 3) Adicionar novo escopo na pilha de escopos, passando o parentID como parâmetro

-- Statement pra interpretar -> valor esperado para o return (se houver) -> estatdo atual -> novo estado
runStatement :: Statement -> Maybe VarType -> OWLState -> (OWLState, Maybe VarValue)
runStatement (VarDec dec) _ state = (addLocalDec dec state, Nothing)
runStatement (Return expr) Nothing state = (state, Nothing) -- TODO: é pra dar erro
runStatement (Return expr) (Just t) state = (state, Nothing) -- TODO: é pra calcular a expr, comparar o tipo resultante dela com o t e retornar o valor dela no lugar no Nothing
runStatement stmt t state = (state, Nothing) -- TODO: definir o que fazer para cada tipo de stmt definido no ProgramTree

-- Calcula o valor da expressão
evaluateExpr :: Expr -> (VarValue, VarType)
evaluateExpr expr = (NumberValue 0, AtomicType "int") -- TODO