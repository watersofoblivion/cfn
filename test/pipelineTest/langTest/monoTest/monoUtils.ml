open CommonTest

let types = [
  Mono.ty_bool;
  Mono.ty_int;
  Mono.ty_long;
  Mono.ty_float;
  Mono.ty_double;
  Mono.ty_rune;
  Mono.ty_string;
]

let valid_promotions = [
  (Mono.ty_int, Mono.ty_long);
  (Mono.ty_int, Mono.ty_double);
  (Mono.ty_float, Mono.ty_double);
]

let assert_ty_bound = EnvTest.assert_bound TypeTest.assert_ty_equal
