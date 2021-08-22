type t = private {
  api:        Api_t.t option;
  docs:       Doc_t.s option;
  examples:   Example_t.s option;
  paginators: Paginator_t.s option;
  smoke:      Smoke_t.t option;
  waiters:    Waiter_t.s option;
}

val of_dir : string -> t
