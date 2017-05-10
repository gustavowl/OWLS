{
module Tokens where
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters
@decimal     = $digit+
@exponent    = [eE] [\-\+] @decimal

tokens :-

  $white+                              ;
  "#".*                                ; -- comentario de linha
  main                                 { \p s -> newToken p Main }
  func                                 { \p s -> newToken p Func }
  proc                                 { \p s -> newToken p Proc }
  ":"                                  { \p s -> newToken p Colon }
  ";"                                  { \p s -> newToken p SemiColon }
  ","                                  { \p s -> newToken p Comma }
  "("                                  { \p s -> newToken p LParen }
  ")"                                  { \p s -> newToken p RParen }
  "{"                                  { \p s -> newToken p LBrace }
  "}"                                  { \p s -> newToken p RBrace }
  int                                  { \p s -> newToken p (Id s) }
  nat                                  { \p s -> newToken p (Id s) }
  real                                 { \p s -> newToken p (Id s) }
  "="                                  { \p s -> newToken p Assign}
  "/"                                  { \p s -> newToken p Divide}
  "*"                                  { \p s -> newToken p Times}
  "-"                                  { \p s -> newToken p Minus}
  "+"                                  { \p s -> newToken p Plus}
  "%"                                  { \p s -> newToken p Modulus}
  "||"                                 { \p s -> newToken p Or_cir}
  "|"                                  { \p s -> newToken p Or}
  "&&"                                 { \p s -> newToken p And_cir}
  "&"                                  { \p s -> newToken p And}
  "=="                                 { \p s -> newToken p Equals}
  "!="                                 { \p s -> newToken p Not_Equals}
  "<"                                  { \p s -> newToken p Less}
  ">"                                  { \p s -> newToken p Greater}
  "<="                                 { \p s -> newToken p LessEq}
  ">="                                 { \p s -> newToken p GreaterEq}
  "!"                                  { \p s -> newToken p Exclamation}
  true                                 { \p s -> newToken p (Bool True) }
  false                                { \p s -> newToken p (Bool False) }
  if                                   { \p s -> newToken p If}
  else                                 { \p s -> newToken p Else}
  for                                  { \p s -> newToken p For}
  while                                { \p s -> newToken p While}
  switch                               { \p s -> newToken p Switch}
  case                                 { \p s -> newToken p Case}
  return                               { \p s -> newToken p Return}
  break                                { \p s -> newToken p Break}
  $digit+                              { \p s -> newToken p (Nat (read s)) }
  [\-]?$digit+                         { \p s -> newToken p (Int (read s)) }
  [\-]?$digit+ \. $digit+              { \p s -> newToken p (Real (read s)) }
  $alpha [$alpha $digit \_ \']*        { \p s -> newToken p (Id s) }
  \" $alpha [$alpha $digit ! \_ \']* \"  { \p s -> newToken p (Str s) }

{

-- Each action has type :: String -> Token

-- The token type:
data Token = Token TokenType Int Int deriving (Eq,Show)

data TokenType =
  Main |
  Func |
  Proc |
  Colon   |
  SemiColon |
  Comma |
  LParen |
  RParen |
  LBrace |
  RBrace |
  Assign    | 
  Divide    |
  Times     |
  Minus     |
  Plus      |
  Modulus    |
  Or_cir    |
  Or        |
  And_cir   |
  And       |
  Exclamation |
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
  Bool Bool |
  Id String |
  Nat Int |
  Int Int |
  Real Double |
  Str String
  deriving (Eq,Show)

newToken :: AlexPosn -> TokenType -> Token
newToken (AlexPn a l c) tok = Token tok l c

getTokens :: String -> [Token]
getTokens = alexScanTokens

}