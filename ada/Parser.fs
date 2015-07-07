#light "off"

module Parser

open FParsec
open System.Globalization

open Ast

let ws = spaces

// Ada defines everything by base unicode categories.  This might be a slow way to parse them but it should catch it all.
let unicodeCategory category = many1Satisfy (fun c -> CharUnicodeInfo.GetUnicodeCategory(c) = category)
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
let format_effector : Parser<string,unit> = many1Satisfy (function
		| '\u0009' | '\u000A' | '\u000B' | '\u000C' | '\u000D' | '\u0085' -> true
		| c when CharUnicodeInfo.GetUnicodeCategory(c) = UnicodeCategory.LineSeparator -> true
		| c when CharUnicodeInfo.GetUnicodeCategory(c) = UnicodeCategory.ParagraphSeparator -> true
		| _ -> false)
let other_control : Parser<string,unit> = unicodeCategory UnicodeCategory.Control
let other_private_use : Parser<string,unit> = unicodeCategory UnicodeCategory.PrivateUse
let other_surrogate : Parser<string,unit> = unicodeCategory UnicodeCategory.Surrogate

// Actual rules.
let identifier_start = choice [letter_uppercase; letter_lowercase; letter_titlecase; letter_modifier; letter_other; number_letter]
let identifier_extend = choice [mark_non_spacing; mark_spacing_combining; number_decimal; punctuation_connector]

let identifier = many1Strings2 identifier_start (identifier_start <|> identifier_extend)

let digit : Parser<string,unit> = many1Satisfy (function 
   | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
	| _ -> false)
let deopt = function | Some(a) -> a	| None -> ""
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

let numeric_literal = (attempt based_literal) <|> decimal_literal

let package_name = sepBy identifier (pstring ".") .>> ws //|>> List.map AIdentifier
let library_unit_name = package_name
let use_package_clause = pstring "use" .>> ws >>. sepBy1 package_name (ws >>. pstring "," >>. ws) .>> ws .>> pstring ";"
let use_clause = use_package_clause
let nonlimited_with_clause = pstring "with" .>> ws >>. sepBy1 library_unit_name (ws >>. pstring "," >>. ws) .>> ws .>> pstring ";"
let with_clause = nonlimited_with_clause

//let library_item = library_unit_declaration <|> library_unit_body <|> library_unit_renaming_declaration
//let context_item = with_clause <|> use_clause
//let context_clause = many context_item
//let compilation_unit = context_clause >>. (library_item <|> subunit)

//let compilation = many compilation_unit .>> eof

//let parse str =
	//match run compilation str with
	//| Success(result, _, _) -> printfn "Success: %A" result
	//| Failure(msg, _, _) -> printfn "Failure: %s" msg