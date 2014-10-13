#light "off"

module Parser

open FParsec

open System.Globalization

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
		| '\u1609' | '\u160A' | '\u160B' | '\u160C' | '\u160D' | '\u1685' -> true
		| c when CharUnicodeInfo.GetUnicodeCategory(c) = UnicodeCategory.LineSeparator -> true
		| c when CharUnicodeInfo.GetUnicodeCategory(c) = UnicodeCategory.ParagraphSeparator -> true
		| _ -> false)
let other_control : Parser<string,unit> = unicodeCategory UnicodeCategory.Control
let other_private_use : Parser<string,unit> = unicodeCategory UnicodeCategory.PrivateUse
let other_surrogate : Parser<string,unit> = unicodeCategory UnicodeCategory.Surrogate

let identifier_start = choice [letter_uppercase; letter_lowercase; letter_titlecase; letter_modifier; letter_other; number_letter]
let identifier_extend = choice [mark_non_spacing; mark_spacing_combining; number_decimal; punctuation_connector]

let identifier = many1Strings2 identifier_start (identifier_start <|> identifier_extend)

let package_name = identifier <|> pstring "."
let use_package_clause = pstring "use" .>> ws >>. sepBy1 package_name (ws >>. pstring "," >>. ws) .>> ws .>> pstring ";"
let use_clause = use_package_clause
let basic_declarative_item = use_clause

let program = basic_declarative_item

let parse str =
	match run program str with
	| Success(result, _, _) -> printfn "Success: %A" result
	| Failure(msg, _, _) -> printfn "Failure: %s" msg