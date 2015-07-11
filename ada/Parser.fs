#light "off"

module Parser

open FParsec
open System.Globalization

open Ast

// Ada defines everything by base unicode categories.  This might be a slow way to parse them but it should catch it all.
let unicodeCategory category = satisfy (fun c -> CharUnicodeInfo.GetUnicodeCategory(c) = category) |>> string
let letter_uppercase : Parser<string,unit> = unicodeCategory UnicodeCategory.UppercaseLetter
let letter_lowercase : Parser<string,unit> = unicodeCategory UnicodeCategory.LowercaseLetter
let letter_titlecase : Parser<string,unit> = unicodeCategory UnicodeCategory.TitlecaseLetter
let letter_modifier : Parser<string,unit> = unicodeCategory UnicodeCategory.ModifierLetter
let letter_other : Parser<string,unit> = unicodeCategory UnicodeCategory.OtherLetter
let mark_non_spacing : Parser<string,unit> = unicodeCategory UnicodeCategory.NonSpacingMark
let mark_spacing_combining : Parser<string,unit> = unicodeCategory UnicodeCategory.SpacingCombiningMark
let number_decimal : Parser<string,unit> = unicodeCategory UnicodeCategory.DecimalDigitNumber
let number_letter : Parser<string,unit> = unicodeCategory UnicodeCategory.LetterNumber
let punctuation_connector : Parser<string,unit> = unicodeCategory UnicodeCategory.ConnectorPunctuation
let other_format : Parser<string,unit> = unicodeCategory UnicodeCategory.Format
let separator_space : Parser<string,unit> = unicodeCategory UnicodeCategory.SpaceSeparator
let separator_line : Parser<string,unit> = unicodeCategory UnicodeCategory.LineSeparator
let separator_paragraph : Parser<string,unit> = unicodeCategory UnicodeCategory.ParagraphSeparator
let format_effector_check = (function
	| '\u0009' | '\u000A' | '\u000B' | '\u000C' | '\u000D' | '\u0085' -> true
	| c when CharUnicodeInfo.GetUnicodeCategory(c) = UnicodeCategory.LineSeparator -> true
	| c when CharUnicodeInfo.GetUnicodeCategory(c) = UnicodeCategory.ParagraphSeparator -> true
	| _ -> false)
let format_effector : Parser<string,unit> = satisfy format_effector_check |>> string
let format_effector_minus_tab : Parser<string,unit> = satisfy (function
	| '\u0009' -> true
	| c -> format_effector_check c) |>> string
let other_control : Parser<string,unit> = unicodeCategory UnicodeCategory.Control
let other_private_use : Parser<string,unit> = unicodeCategory UnicodeCategory.PrivateUse
let other_surrogate : Parser<string,unit> = unicodeCategory UnicodeCategory.Surrogate
let delimiter_check = (function
	| '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-' | '.'
	| '/' | ':' | ';' | '<' | '=' | '>' | '|' -> true
	| _ -> false)
let compound_delimiter : Parser<string,unit> = choice [ pstring "=>"; pstring ".."; pstring "**";
	pstring ":="; pstring "/="; pstring ">="; pstring "<="; pstring "<<"; pstring ">>"; pstring "<>" ]
let delimiter = compound_delimiter <|> (satisfy delimiter_check |>> string) <?> "delimiter" |>> LDelimiter

let graphic_character_check = (function
	| c when CharUnicodeInfo.GetUnicodeCategory(c) = UnicodeCategory.Control -> false
	| c when CharUnicodeInfo.GetUnicodeCategory(c) = UnicodeCategory.PrivateUse -> false
	| c when CharUnicodeInfo.GetUnicodeCategory(c) = UnicodeCategory.Surrogate -> false
	| c when format_effector_check c -> false
	| '\uFFFE' | '\uFFFF' -> false
	| _ -> true)
let graphic_character : Parser<string,unit> = satisfy graphic_character_check |>> string
let non_quotation_mark_graphic_character : Parser<string,unit> = satisfy (function
	| '"' -> false
	| _ as c -> graphic_character_check c) |>> string

// Utility
let deopt = function | Some(a) -> a | None -> ""
let eol : Parser<string,unit> = many1Strings format_effector_minus_tab
let sep : Parser<unit,unit> = skipMany (choice [ eol; separator_space; format_effector; other_format ])
let sep1 : Parser<unit,unit> = skipMany1 (choice [ eol; separator_space; format_effector; other_format ])

// Basic lexical units.  They return themselves since I want to be able to regenerate source from AST.
let keyword_or_identifier (x : string) = match x.ToLower() with
	| "abort" | "abs" | "abstract" | "accept" | "access" | "aliased" | "all" | "and" | "array"
	| "at" | "begin" | "body" | "case" | "constant" | "declare" | "delay" | "delta" | "digits" | "do"
	| "else" | "elsif" | "end" | "entry" | "exception" | "exit" | "for" | "function" | "generic" | "goto"
	| "if" | "in" | "interface" | "is" | "limited" | "loop" | "mod" | "new" | "not" | "null" | "of" | "or"
	| "others" | "out" | "overriding" | "package" | "pragma" | "private" | "procedure" | "protected"
	| "raise" | "range" | "record" | "rem" | "renames" | "requeue" | "return" | "reverse" | "select"
	| "separate" | "some" | "subtype" | "synchronized" | "tagged" | "task" | "terminate" | "then"
	| "type" | "until" | "use" | "when" | "while" | "with" | "xor" -> LKeyword(x)
	| _ -> LIdentifier(x)
let identifier_start = choice [letter_uppercase; letter_lowercase; letter_titlecase; letter_modifier; letter_other; number_letter]
let identifier_extend = choice [mark_non_spacing; mark_spacing_combining; number_decimal; punctuation_connector]

let identifier = many1Strings2 identifier_start (identifier_start <|> identifier_extend) <?> "identifier" |>> keyword_or_identifier

let digit : Parser<string,unit> = many1Satisfy (function
	| '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
	| _ -> false)
let numeral : Parser<string,unit> = many1Strings2 digit (digit <|> many1Strings2 (pstring "_") digit)
let exponent : Parser<string,unit> = many1Strings2 ((pstring "E") <|> (pstring "e"))
	(pipe2 (opt (pstring "+")) numeral (fun a b -> (deopt a) + b)	<|>
	(pipe2 (pstring "-") numeral (fun a b -> a + b)))
let decimal_literal : Parser<string,unit> = pipe3 numeral
	(opt (pipe2 (pstring ".") numeral (fun a b -> a + b)))
	(opt exponent) (fun a b c -> a + (deopt b) + (deopt c))

let extended_digit : Parser<string,unit> = many1Satisfy (function
	| '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
	| 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
	| 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> true
	| _ -> false)
let based_numeral : Parser<string,unit> = many1Strings2 extended_digit
	(extended_digit <|> many1Strings2 (pstring "_") extended_digit)
let based_literal = pipe5 numeral (pstring "#") based_numeral
	(opt (pipe2 (pstring ".") based_numeral (fun a b -> a + b)))
	(pipe2 (pstring "#") (opt exponent) (fun a b -> a + (deopt b)))
	(fun a b c e f -> a + b + c + (deopt e) + f)

let numeric_literal = (attempt based_literal) <|> decimal_literal <?> "numeric literal" |>> LNumericLiteral

let character_literal = pipe3 (pstring "'") graphic_character (pstring "'") (fun a b c -> a + b + c) <?> "character literal" |>> LCharacterLiteral

let string_element = (pstring "\"\"") <|> non_quotation_mark_graphic_character
let string_literal = pipe3 (pstring "\"") (manyStrings string_element) (pstring "\"") (fun a b c -> a + b + c) <?> "string literal" |>> LStringLiteral

let comment = pipe2 (pstring "--") (restOfLine false) (fun a b -> a + b) <?> "comment" |>> LComment

let lexical_element : Parser<LexicalElement,unit> = choice [ comment; delimiter; (attempt numeric_literal); identifier; character_literal; string_literal ]
let compilation = many (sep >>. lexical_element .>> sep) .>> eof

let parse str =
	match run compilation str with
	| Success(result, _, _) -> printfn "Success: %A" result
	| Failure(msg, _, _) -> printfn "Failure: %s" msg