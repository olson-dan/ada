#light "off"

open System
open System.Reflection
open System.Reflection.Emit
open FParsec

// http://www.infres.enst.fr/~pautet/Ada95/chap02.htm
let program = """
with Ada.Text_IO;
use Ada.Text_IO;

procedure UglyForm is begin
	Put ("Good form ");
	Put ("can aid in ");
	Put ("understanding a program,");
	New_Line;
	Put ("and bad form ");
	Put ("can make a program ");
	Put ("unreadable.");
	New_Line;
end UglyForm;
"""

type AExpression =
	| ACall of string * AValue list
	| AMethod of string * AExpression list
and AValue =
	| AString of string

type AResult =
	| ASuccess
	| AFailure of string

let ConsoleType = typeof<System.Console>
let StringType = typeof<string>
let BindFlags = BindingFlags.Public ||| BindingFlags.Static

let Ada_Text_IO_Put (il : ILGenerator) (args : AValue list) =  match args with
	| AString(s) :: [] ->
		il.Emit(OpCodes.Ldstr, s);
		il.Emit(OpCodes.Call, ConsoleType.GetMethod("Write", BindFlags, null, [| StringType |], null));
		ASuccess
	| _ -> AFailure ("Ada.Text_IO.Put takes a single string as an argument.")

let Ada_Text_IO_New_Line (il : ILGenerator) (args : AValue list) = match args with
	| [] ->
		il.Emit (OpCodes.Ldstr, "\n");
		il.Emit(OpCodes.Call, ConsoleType.GetMethod("Write", BindFlags, null, [| StringType |], null));
		ASuccess
	| _ -> AFailure ("Ada.Text_IO.New_Line does not take any arguments.")

let builtins : Map<string, ILGenerator -> AValue list -> AResult> = [
	("Ada.Text_IO.Put", Ada_Text_IO_Put);
	("Ada.Text_IO.New_Line", Ada_Text_IO_New_Line)
] |> Map.ofList

let resolveFunction name =
	builtins.[name]

let mutable entryPoint : MethodBuilder = null

let rec generate (tb : TypeBuilder) (il : ILGenerator) ast =
	match ast with
	| AMethod(name, expressions) :: b ->
		let mb = tb.DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static) in
		let il = mb.GetILGenerator() in
		entryPoint <- mb;
		expressions |> generate tb il;
		il.Emit(OpCodes.Ret);
		b |> generate tb null
	| ACall(name, arguments) :: b ->
		let fcall = resolveFunction name in
		fcall il arguments |> ignore;
		b |> generate tb il
	| [] -> ()

let save name ast =
	let exe = name + ".exe" in
	let asm = AppDomain.CurrentDomain.DefineDynamicAssembly(new AssemblyName(name), AssemblyBuilderAccess.RunAndSave) in
	let modbuilder = asm.DefineDynamicModule (exe, exe) in
	let tb = modbuilder.DefineType("Program", TypeAttributes.Class) in
	generate tb null ast;
	tb.CreateType() |> ignore;
	asm.SetEntryPoint(entryPoint);
	asm.Save (exe)

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

[<EntryPoint>]
let main argv =
	let ast = [ (AMethod ("UglyForm", [
		ACall ("Ada.Text_IO.Put", [ AString ("Hello World") ]);
		ACall ("Ada.Text_IO.New_Line", [ ] ) ] ) ) ] in
	save "TestProgram" ast;
	Console.ReadKey() |> ignore; 0
