#light "off"

module Ast

type AExpression =
	| ACall of string * AValue list
	| AMethod of string * AExpression list
and AValue =
	| AString of string
	| AIdentifier of string

type AResult =
	| ASuccess
	| AFailure of string