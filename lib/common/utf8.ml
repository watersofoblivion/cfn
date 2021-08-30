open Format

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
