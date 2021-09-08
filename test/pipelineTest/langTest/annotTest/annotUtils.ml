open CommonTest

let types = [
  Annot.ty_bool;
  Annot.ty_int;
  Annot.ty_long;
  Annot.ty_float;
  Annot.ty_double;
  Annot.ty_rune;
  Annot.ty_string
]

let valid_promotions = [
  (Annot.ty_int, Annot.ty_long);
  (Annot.ty_int, Annot.ty_double);
  (Annot.ty_float, Annot.ty_double);
]

let assert_ty_bound = EnvTest.assert_bound TypeTest.assert_ty_equal
