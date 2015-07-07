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
		testList "format_effector" (["\t";"\n"] |> List.map (parses format_effector));
	]

let runTests () =
	run tests
