type port = Up | Down | Left | Right | Any | Last

type label = string

type cond = NZ | EZ | GZ | LZ

type operand = Port of port | Nil | Acc | Literal of int

type instr =
  | Nop
  | Swap
  | Save
  | Negate
  | Mov of operand * operand
  | Jmp of label
  | CondJmp of cond * label
  | RelJmp of operand
  | Label of label
  | Add of operand
  | Sub of operand

type prog = int * instr list
