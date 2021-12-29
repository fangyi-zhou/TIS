module List = ListLabels

let get_files dir =
  Sys.readdir dir |> Array.to_list |> List.map ~f:(fun fn -> dir ^ "/" ^ fn)

let has_failures = ref false

let passes = ref 0

let print_exn = function
  | Tis.ParserError (start, finish) ->
      let show_position pos =
        Printf.sprintf "%d:%d" pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
      in
      Printf.sprintf "Parser Error at %s to %s" (show_position start)
        (show_position finish)
  | e -> Printexc.to_string e

let test file =
  try
    ignore (Tis.parse file) ;
    passes := !passes + 1
  with e ->
    has_failures := true ;
    Printf.printf "Test %s failed: %s\n" file (print_exn e)

(* test the parser *)
let () =
  let files = get_files "data" in
  Printf.printf "Found %d tests.\n" (List.length files) ;
  List.iter ~f:test files ;
  Printf.printf "%d tests passed.\n" !passes ;
  exit (if !has_failures then 1 else 0)
