(* Lexing *)

let init_tracking fname lexbuf =
  let default_position =
    { Lexing.pos_fname = fname;
      pos_lnum         = 0;
      pos_bol          = 0;
      pos_cnum         = 0 }
  in
  let _ = default_position in
  (* Lexing.set_filename lexbuf fname; *)
  (* Lexing.set_position lexbuf default_position; *)
  lexbuf

let from_string s =
  Lexing.from_string ~with_positions:true s
    |> init_tracking "-"

let from_channel ?fname:(fname = "-") ic =
  Lexing.from_channel ~with_positions:true ic
    |> init_tracking fname
