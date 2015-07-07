#light "off"

open System

open Ast
open Parser
open Emit
open Tests

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


[<EntryPoint>]
let main argv =
	runTests () |> ignore;
	//parse program;
//	let ast = [ (AMethod ("UglyForm", [
//		ACall ("Ada.Text_IO.Put", [ AString ("Hello World") ]);
//		ACall ("Ada.Text_IO.New_Line", [ ] ) ] ) ) ] in
//	saveBinary "TestProgram" ast; 0
	Console.ReadKey() |> ignore; 0
