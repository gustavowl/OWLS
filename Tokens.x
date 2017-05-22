{
module Tokens where
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters



$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]
$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']
$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]
$graphic   = [$small $large $symbol $digit $special \:\"\']
$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

@reservedid = 
  as|case|class|data|default|deriving|do|else|hiding|if|
  import|in|infix|infixl|infixr|instance|let|module|newtype|
  of|qualified|then|type|where
@reservedop =
  ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"
@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal
$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
   | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
   | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
   | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

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
  bool                                 { \p s -> newToken p (Id s) }
  nat                                  { \p s -> newToken p (Id s) }
  int                                  { \p s -> newToken p (Id s) }
  real                                 { \p s -> newToken p (Id s) }
  char                                 { \p s -> newToken p (Id s) }
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
  $digit+                              { \p s -> newToken p (Nat (read s))}
  [\-]?@decimal+                       { \p s -> newToken p (Int (read s))}
  [\-]?@decimal+ \. @decimal+          { \p s -> newToken p (Real (read s))}
  $alpha [$alpha $digit \_ \']*        { \p s -> newToken p (Id s)}
  \" @string* \"                       {\p s -> newToken p (String (read s))}
  \' @string? \'                       {\p s -> newToken p (Char (read s))}

{

-- Each action has type :: String -> Token

-- The token type:
data Token = Token TokenSymbol Int Int deriving (Eq,Show)

data TokenSymbol =
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
  Nat Double |
  Int Double |
  Real Double |
  Char Char |
  String String 
  deriving (Eq,Show)

newToken :: AlexPosn -> TokenSymbol -> Token
newToken (AlexPn a l c) tok = Token tok l c

getTokens :: String -> [Token]
getTokens = alexScanTokens

}