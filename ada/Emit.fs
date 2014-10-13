#light "off"

module Emit

open System
open System.Reflection
open System.Reflection.Emit

open Ast
open Builtins

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

let saveBinary name ast =
	let exe = name + ".exe" in
	let asm = AppDomain.CurrentDomain.DefineDynamicAssembly(new AssemblyName(name), AssemblyBuilderAccess.RunAndSave) in
	let modbuilder = asm.DefineDynamicModule (exe, exe) in
	let tb = modbuilder.DefineType("Program", TypeAttributes.Class) in
	generate tb null ast;
	tb.CreateType() |> ignore;
	asm.SetEntryPoint(entryPoint);
	asm.Save (exe)
