{
  open Lexing
  open Parser

  exception LexError of string
}

rule line_comment = parse
   | '\n' { new_line lexbuf ; token lexbuf }
   | _ { line_comment lexbuf }

and token = parse
   (* whitespace *)
   | ('\t'|' ')+ { token lexbuf }
   | ('\n'|'\r') { new_line lexbuf ; token lexbuf }

   (* comments *)
   | "#" { line_comment lexbuf }

   (* integers *)
   | '-'?['0'-'9']+ as i { INT (int_of_string i) }

   (* symbols *)
   | ',' { COMMA }
   | ':' { COLON }
   | '@' { ARROBA }

   (* keywords *)
   | "ACC" { ACC_KW }
   | "NIL" { NIL_KW }
   | "LEFT" { LEFT_KW }
   | "RIGHT" { RIGHT_KW }
   | "UP" { UP_KW }
   | "DOWN" { DOWN_KW }
   | "ANY" { ANY_KW }
   | "LAST" { LAST_KW }
   | "NOP" { NOP_KW }
   | "MOV" { MOV_KW }
   | "SAV" { SAV_KW }
   | "SWP" { SWP_KW }
   | "ADD" { ADD_KW }
   | "SUB" { SUB_KW }
   | "NEG" { NEG_KW }
   | "JMP" { JMP_KW }
   | "JEZ" { JEZ_KW }
   | "JNZ" { JNZ_KW }
   | "JGZ" { JGZ_KW }
   | "JLZ" { JLZ_KW }
   | "JRO" { JRO_KW }

   (* hidden command *)
   | "HCF" { HCF_KW }

   (* labels *)
   | ['A'-'Z''0'-'9']+ as s { LABEL s }

   | eof { EOI }
   | _ as unrecog {
     let offset = Lexing.lexeme_start lexbuf in
     let str = Printf.sprintf "At offset %d: unexpected character('%c').\n" offset unrecog in
     LexError str |> raise }
