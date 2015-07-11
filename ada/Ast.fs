#light "off"

module Ast

type LexicalElement =
	| LDelimiter of string
	| LIdentifier of string
	| LNumericLiteral of string
	| LComment of string
	| LStringLiteral of string
	| LCharacterLiteral of string
	| LKeyword of string

type AExpression =
	| ACall of string * AValue list
	| AMethod of string * AExpression list
and AValue =
	| AString of string
	| AIdent of string

type AResult =
	| ASuccess
	| AFailure of string