#light "off"

module Parser

open FParsec

(*
type Json =
	| JString of string
	| JNumber of float
	| JBool of bool
	| JNull
	| JList of Json list
	| JObject of Map<string, Json>

let ws = spaces
let jnull : Parser<Json,unit> = stringReturn "null" JNull
let jbool : Parser<Json,unit> = (stringReturn "true" (JBool true)) <|> (stringReturn "false" (JBool false))
let jnumber : Parser<Json,unit> = pfloat |>> JNumber
let stringLiteral : Parser<string,unit> =
	let escape = anyOf "\"\\/bfnrt" |>> function
		| 'b' -> "\b"
		| 'f' -> "\u000C"
		| 'n' -> "\n"
		| 'r' -> "\r"
		| 't' -> "\t"
		| c -> string c in
	let unicodeEscape =
		let hex2int c = (int c &&& 15) + (int c >>> 6) * 9 in
		pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
		    (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0 |> char |> string
        ) in
	let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape) in
	let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\') in
	between (pstring "\"") (pstring "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)

let jstring : Parser<Json,unit> = stringLiteral |>> JString

let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()

let listBetweenStrings sopen sclose pelement f =
	between (pstring sopen) (pstring sclose) (ws >>. sepBy (pelement .>> ws) (pstring "," >>. ws) |>> f)
let jlist = listBetweenStrings "[" "]" jvalue JList

let keyValue = stringLiteral .>>. (ws >>. pstring ":" >>. ws >>. jvalue)
let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

do jvalueRef := choice [jobject; jlist; jstring; jnumber; jbool; jnull]
let json = ws >>. jvalue .>> ws .>> eof
*)

let print x = printfn "%A" x; x

let parse p str =
	match run p str with
	| Success(result, _, _) -> printfn "Success: %A" result
	| Failure(msg, _, _) -> printfn "Failure: %s" msg