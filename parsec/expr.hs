import Text.ParserCombinators.Parsec

expr = sum
sum = chainl1 sum_term add_op
sum_term = mul
mul = chainl1 mul_term mul_op
mul_term = end_term
end_term = parens expr
	<|> num
	<|> id
	<|> neg id
	<|> func_call

add_op = do { symbol "+"; return (+) }
	<|> do { symbol "-"; return (-) }

mul_op = do { symbol "*"; return (*) }
	<|> do { symbol "/"; return (div) }

num = Double <|> Integer
