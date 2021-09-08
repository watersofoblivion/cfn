open CommonTest

let types = [
  Clos.ty_bool;
  Clos.ty_int;
  Clos.ty_long;
  Clos.ty_float;
  Clos.ty_double;
  Clos.ty_rune;
  Clos.ty_string;
]

let valid_promotions = [
  (Clos.ty_int, Clos.ty_long);
  (Clos.ty_int, Clos.ty_double);
  (Clos.ty_float, Clos.ty_double);
]

let assert_ty_bound = EnvTest.assert_bound TypeTest.assert_ty_equal
