exception ParserError of (Lexing.position * Lexing.position)

let parse_from_lexbuf lexbuf =
  try Parser.program Lexer.token lexbuf with
  | Lexer.LexError msg -> raise (Lexer.LexError msg)
  | Parser.Error ->
      let err_interval =
        (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      raise (ParserError err_interval)

let parse fname =
  let ch = open_in fname in
  let lexbuf = Lexing.from_channel ch in
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname= fname} ;
  let ret = parse_from_lexbuf lexbuf in
  close_in ch ; ret

let parse_string string = parse_from_lexbuf @@ Lexing.from_string string
