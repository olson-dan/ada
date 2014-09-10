#light "off"

open System
open Microsoft.FSharp.Text.Lexing

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
	Put("unreadable.");
	New_Line;
end UglyForm;

-- Result of execution

-- Good form can aid in understanding a program,
-- and bad form can make a program unreadable.
"""

let print x = printfn "%A" x; x

let parse text =
	let lexbuf = LexBuffer<char>.FromString text in
	while (Lexer.tokenstream lexbuf |> print) <> Parser.EOF do () done

[<EntryPoint>]
let main argv = 
	program |> print |> parse |> print |> ignore;
	Console.ReadKey() |> ignore; 0