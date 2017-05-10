{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters
@decimal     = $digit+
@exponent    = [eE] [\-\+] @decimal

tokens :-

  $white+                              ;
  "#".*                                ; -- comentario de linha
  main                                 { \s -> Main }
  func                                 { \s -> Func }
  proc                                 { \s -> Proc }
  var                                  { \s -> Var }
  ":"                                  { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  "("                                  { \s -> LParen}
  ")"                                  { \s -> RParen}
  "{"                                  { \s -> LBrace}
  "}"                                  { \s -> RBrace}
  int                                  { \s -> Type s}
  nat                                  { \s -> Type s}
  real                                 { \s -> Type s}
  "="                                  { \s -> Assign}
  "/"                                  { \s -> Divide}
  "*"                                  { \s -> Times}
  "-"                                  { \s -> Minus}
  "+"                                  { \s -> Plus}
  "%"                                  { \s -> Module}
  "||"                                 { \s -> Or_cir}
  "|"                                  { \s -> Or}
  "&&"                                 { \s -> And_cir}
  "&"                                  { \s -> And}
  "=="                                 { \s -> Equals}
  "!="                                 { \s -> Not_Equals}
  "<"                                  { \s -> Less}
  ">"                                  { \s -> Greater}
  "<="                                 { \s -> LessEq}
  ">="                                 { \s -> GreaterEq}
  if                                   { \s -> If}
  else                                 { \s -> Else}
  for                                  { \s -> For}
  while                                { \s -> While}
  switch                               {\s -> Switch}
  case                                 {\s -> Case}
  return                               { \s -> Return}
  break                                { \s -> Break}
  then                                 { \s -> Then}
  print                                { \s -> Write}
  $digit+                              { \s -> Nat (read s) }
  [\-]?$digit+                         { \s -> Int (read s) }
  [\-]?$digit+ \. $digit+              { \s -> Real (read s)}
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"  { \s -> String s}




{

-- Each action has type :: String -> Token

-- The token type:
data Token =
  Main |
  Func |
  Proc |
  Var     |
  Colon   |
  SemiColon |
  LParen |
  RParen |
  LBrace |
  RBrace |
  Assign    | 
  Divide    |
  Times     |
  Minus     |
  Plus      |
  Module    |
  Or_cir    |
  Or        |
  And_cir   |
  And       |
  Equals    |
  Not_Equals |
  Less      |
  Greater    |
  LessEq |
  GreaterEq |
  If  |
  Else |  
  For |
  While |
  Switch |
  Case   |
  Return |
  Break  |
  Then |
  Write |
  Type String |
  Id String |
  Nat Int |
  Int Int |
  Real Double |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
