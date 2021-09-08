open CommonTest

let types = [
  Ir.ty_bool;
  Ir.ty_int;
  Ir.ty_long;
  Ir.ty_float;
  Ir.ty_double;
  Ir.ty_rune;
  Ir.ty_string;
]

let valid_promotions = [
  (Ir.ty_int, Ir.ty_long);
  (Ir.ty_int, Ir.ty_double);
  (Ir.ty_float, Ir.ty_double);
]

let assert_ty_bound = EnvTest.assert_bound TypeTest.assert_ty_equal
