module State where

-- GlobalScope, ScopeStack, 
type OWLState = (OWLScope, [OWLScope]

-- Name table
type OWLScope = ([Function], [Procedure], [Var])