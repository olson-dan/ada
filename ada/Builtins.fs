#light "off"

module Builtins

open System.Reflection
open System.Reflection.Emit

open Ast

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

