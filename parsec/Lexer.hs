module Lexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

language = emptyDef {
	Token.commentStart    	= "##",
	Token.commentEnd      	= "##",
	Token.commentLine		= "#",
	Token.identStart      	= letter,
	Token.identLetter     	= alphaNum,
	Token.reservedNames   	= [ "func",
								"proc",
								"if",
                                "else",
                                "while",
                                "for",
                                "true",
                                "false",
                                "not",
                                "and",
                                "or" ],
	Token.reservedOpNames 	= [ "+", 
								"-", 
								"*", 
								"/", 
								"=",
								"<", 
								">", 
								"and", 
								"or", 
								"not" ]
}

lexer = Token.makeTokenParser language

identifier		= Token.identifier 		lexer
reserved 		= Token.reserved   		lexer
reservedOp 		= Token.reservedOp 		lexer
parens 			= Token.parens     		lexer
braces 			= Token.braces			lexer
integer 		= Token.integer    		lexer
semi 			= Token.semi       		lexer
whiteSpace 		= Token.whiteSpace 		lexer
symbol 			= Token.symbol			lexer
comma 			= Token.comma 			lexer
colon 			= Token.colon			lexer
stringLiteral 	= Token.stringLiteral	lexer