%token <string>LABEL
%token <int>INT

%token EOI

%token COMMA
%token COLON
%token ARROBA

(* Ports and registers *)
%token ACC_KW
%token NIL_KW
%token LEFT_KW
%token RIGHT_KW
%token UP_KW
%token DOWN_KW
%token ANY_KW
%token LAST_KW

(* Instructions *)
%token NOP_KW
%token MOV_KW
%token SAV_KW
%token SWP_KW
%token ADD_KW
%token SUB_KW
%token NEG_KW
%token JMP_KW
%token JEZ_KW
%token JNZ_KW
%token JGZ_KW
%token JLZ_KW
%token JRO_KW

%token HCF_KW

%start <Syntax.prog list> program
%{
  open Syntax
%}
%%

program:
  | prog = node* ; EOI ; { prog }

node:
  | ARROBA; node = INT; instrs = instrs* ; { (node, instrs) }

instrs:
  | NOP_KW { Nop }
  | SWP_KW { Swap }
  | SAV_KW { Save }
  | NEG_KW { Negate }
  | HCF_KW { failwith "HCF" }
  | MOV_KW ; op1 = operand; COMMA? ; op2 = operand { Mov (op1, op2) }
  | JRO_KW ; op = operand { RelJmp op }
  | ADD_KW ; op = operand { Add op }
  | SUB_KW ; op = operand { Sub op }
  | label = label ; COLON ; { Label label }
  | JMP_KW ; label = label { Jmp label }
  | JEZ_KW ; label = label { CondJmp (EZ, label) }
  | JNZ_KW ; label = label { CondJmp (NZ, label) }
  | JGZ_KW ; label = label { CondJmp (GZ, label) }
  | JLZ_KW ; label = label { CondJmp (LZ, label) }

label:
  | label = LABEL { label }
  (* Sometimes people use integers as labels *)
  | i = INT { string_of_int i }
  (* Sometimes people use keywords as labels *)
  | ACC_KW { "ACC" }
  | NIL_KW { "NIL" }
  | LEFT_KW { "LEFT" }
  | RIGHT_KW { "RIGHT" }
  | UP_KW { "UP" }
  | DOWN_KW { "DOWN" }
  | ANY_KW { "ANY" }
  | LAST_KW { "LAST" }
  | NOP_KW { "NOP" }
  | MOV_KW { "MOV" }
  | SAV_KW { "SAV" }
  | SWP_KW { "SWP" }
  | ADD_KW { "ADD" }
  | SUB_KW { "SUB" }
  | NEG_KW { "NEG" }
  | JMP_KW { "JMP" }
  | JEZ_KW { "JEZ" }
  | JNZ_KW { "JNZ" }
  | JGZ_KW { "JGZ" }
  | JLZ_KW { "JLZ" }
  | JRO_KW { "JRO" }

port:
  | UP_KW { Up }
  | DOWN_KW { Down }
  | LEFT_KW { Left }
  | RIGHT_KW { Right }
  | ANY_KW { Any }
  | LAST_KW { Last }

operand:
  | port = port ; { Port port }
  | NIL_KW ; { Nil }
  | ACC_KW ; { Acc }
  | literal = INT ; { Literal literal }

