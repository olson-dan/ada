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

let package_name = sepBy identifier (pstring ".") .>> ws //|>> List.map AIdentifier
let library_unit_name = package_name
let use_package_clause = pstring "use" .>> ws >>. sepBy1 package_name (ws >>. pstring "," >>. ws) .>> ws .>> pstring ";"
let use_clause = use_package_clause
let nonlimited_with_clause = pstring "with" .>> ws >>. sepBy1 library_unit_name (ws >>. pstring "," >>. ws) .>> ws .>> pstring ";"
let with_clause = nonlimited_with_clause

let library_item = library_unit_declaration <|> library_unit_body <|> library_unit_renaming_declaration
let context_item = with_clause <|> use_clause
let context_clause = many context_item
let compilation_unit = context_clause >>. (library_item <|> subunit)

let compilation = many compilation_unit .>> eof

let parse str =
	match run compilation str with
	| Success(result, _, _) -> printfn "Success: %A" result
	| Failure(msg, _, _) -> printfn "Failure: %s" msg