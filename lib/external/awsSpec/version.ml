type t = {
  api:        Api_t.t option;
  docs:       Doc_t.s option;
  examples:   Example_t.s option;
  paginators: Paginator_t.s option;
  smoke:      Smoke_t.t option;
  waiters:    Waiter_t.s option;
}

let of_dir dir =
  let of_file parser fname =
    fname |> Filename.concat dir |> External.Parse.json_file parser
  in

  { api        = of_file Api_j.read_t "api-2.json";
    docs       = of_file Doc_j.read_s "docs-2.json";
    examples   = of_file Example_j.read_s "examples-1.json";
    paginators = of_file Paginator_j.read_s "paginators-1.json";
    smoke      = of_file Smoke_j.read_t "smoke.json";
    waiters    = of_file Waiter_j.read_s "waiters-2.json" }
