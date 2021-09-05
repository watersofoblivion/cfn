open Format

let single_quote = Uchar.of_char '\''

let to_string uchar =
  let buf = Buffer.create 1 in
  Buffer.add_utf_8_uchar buf uchar;
  buf
    |> Buffer.contents


let length str =
  let len len _ = function
    | `Uchar _ -> len + 1
    | `Malformed msg ->
      let msg = sprintf "Malformed UTF-8 string: %S" msg in
      Invalid_argument msg
        |> raise
  in
  Uutf.String.fold_utf_8 len 0 str

let normalize = Uunf_string.normalize_utf_8 `NFC
