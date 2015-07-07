#light "off"

module Tests

open System
open Fuchu
open Parser

open System.Globalization

let parses p x = testCase (sprintf "with input %s" x) <|
	fun _ -> Assert.Equal("", x, match FParsec.CharParsers.run p x with
		| FParsec.CharParsers.ParserResult.Success(result,_,_) -> result
		| FParsec.CharParsers.ParserResult.Failure(msg,_,_) -> msg)
 
let tests =
	testList "parser tests" [
		testList "identifier" (["Count";"X";"Get_Symbol";"Ethelyn";"Marion";"Snobol_4";"X1";"Page_Count";"Store_Next_Item"]
			|> List.map (parses identifier));
		testList "exponent" (["E12_34"; "E+12_34"; "E-12_34"; "E1234"]
			|> List.map (parses exponent));
		testList "decimal_literal" (["12";"0";"1E6";"123_456";"12.0";"0.0";"0.456";"3.14159_26"]
			|> List.map (parses decimal_literal));
		testList "based_literal" (["2#1111_1111#";"16#FF#";"016#0ff#";"16#E#E1";"2#1110_0000#";"16#F.FF#E+2";"2#1.1111_1111_1110#E11"]
			|> List.map (parses based_literal));
		testList "numeric_literal" (["12";"0";"1E6";"123_456";"12.0";"0.0";"0.456";"3.14159_26";
			"2#1111_1111#";"16#FF#";"016#0ff#";"16#E#E1";"2#1110_0000#";"16#F.FF#E+2";"2#1.1111_1111_1110#E11"]
			|> List.map (parses numeric_literal));
		testList "character_literal" (["'A'"; "'*'"; "'''"; "' '"; "'L'"; "'Л'"; "'Λ'"; "'∞'"; "'א'"]
			|> List.map (parses character_literal));
		testList "string_literal" (["\"Message of the day:\""; "\"\""; "\" \""; "\"A\""; "\"\"\"\""]
			|> List.map (parses string_literal));
		testList "format_effector" (["\t";"\n"] |> List.map (parses format_effector));
	]

let runTests () =
	run tests
