let json_file parser path =
  match Sys.file_exists path with
    | false -> None
    | true ->
      let ic = open_in path in
      let finally _ = close_in ic in

      let parse _ =
        let fname = Filename.basename path in
        let lexer_state = Yojson.init_lexer ~fname () in
        let lexbuf = Lexing.from_channel ic in
        parser lexer_state lexbuf
      in

      let json = Fun.protect ~finally parse in
      Some json
