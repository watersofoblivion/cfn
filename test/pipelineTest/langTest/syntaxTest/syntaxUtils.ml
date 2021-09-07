open CommonTest

let deloc_optional deloc = function
  | Some value -> Some (deloc value)
  | None -> None

let assert_loc extract constr ctxt =
  let loc = LocTest.gen () in
  loc
    |> constr
    |> extract
    |> LocTest.assert_loc_equal ~ctxt loc
