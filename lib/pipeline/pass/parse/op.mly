%{
  [@@@coverage exclude_file]
  open Common

  let make_op constr (start_loc, end_loc) env kontinue =
    Loc.loc start_loc end_loc
      |> constr
      |> kontinue env

  let make_un_neg loc env kontinue = make_op Syntax.un_neg loc env kontinue
  let make_un_log_not loc env kontinue = make_op Syntax.un_log_not loc env kontinue
  let make_un_bit_not loc env kontinue = make_op Syntax.un_bit_not loc env kontinue

  let make_bin_struct_eq loc env kontinue = make_op Syntax.bin_struct_eq loc env kontinue
  let make_bin_struct_neq loc env kontinue = make_op Syntax.bin_struct_neq loc env kontinue
  let make_bin_phys_eq loc env kontinue = make_op Syntax.bin_phys_eq loc env kontinue
  let make_bin_phys_neq loc env kontinue = make_op Syntax.bin_phys_neq loc env kontinue
  let make_bin_lt loc env kontinue = make_op Syntax.bin_lt loc env kontinue
  let make_bin_lte loc env kontinue = make_op Syntax.bin_lte loc env kontinue
  let make_bin_gt loc env kontinue = make_op Syntax.bin_gt loc env kontinue
  let make_bin_gte loc env kontinue = make_op Syntax.bin_gte loc env kontinue
  let make_bin_add loc env kontinue = make_op Syntax.bin_add loc env kontinue
  let make_bin_sub loc env kontinue = make_op Syntax.bin_sub loc env kontinue
  let make_bin_mul loc env kontinue = make_op Syntax.bin_mul loc env kontinue
  let make_bin_div loc env kontinue = make_op Syntax.bin_div loc env kontinue
  let make_bin_mod loc env kontinue = make_op Syntax.bin_mod loc env kontinue
  let make_bin_exp loc env kontinue = make_op Syntax.bin_exp loc env kontinue
  let make_bin_log_and loc env kontinue = make_op Syntax.bin_log_and loc env kontinue
  let make_bin_log_or loc env kontinue = make_op Syntax.bin_log_or loc env kontinue
  let make_bin_bit_and loc env kontinue = make_op Syntax.bin_bit_and loc env kontinue
  let make_bin_bit_or loc env kontinue = make_op Syntax.bin_bit_or loc env kontinue
  let make_bin_bit_xor loc env kontinue = make_op Syntax.bin_bit_xor loc env kontinue
%}

/* Testing Entry Points */

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.un -> 'a) -> 'a>  un_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.bin -> 'a) -> 'a> bin_test
%start un_test
%start bin_test

%%

/* Test Entry Points */

un_test:
| op = un_op; EOF { op }

bin_test:
| op = bin_op; EOF { op }

/* Operators */

%public %inline un_op:
| "-" { make_un_neg $sloc }
| "!" { make_un_log_not $sloc }
| "~" { make_un_bit_not $sloc }

%public %inline bin_op:
| "=="  { make_bin_struct_eq $sloc }
| "!="  { make_bin_struct_neq $sloc }
| "===" { make_bin_phys_eq $sloc }
| "!==" { make_bin_phys_neq $sloc }
| "<"   { make_bin_lt $sloc }
| "<="  { make_bin_lte $sloc }
| ">"   { make_bin_gt $sloc }
| ">="  { make_bin_gte $sloc }
| "+"   { make_bin_add $sloc }
| "-"   { make_bin_sub $sloc }
| "*"   { make_bin_mul $sloc }
| "/"   { make_bin_div $sloc }
| "%"   { make_bin_mod $sloc }
| "^^"  { make_bin_exp $sloc }
| "&&"  { make_bin_log_and $sloc }
| "||"  { make_bin_log_or $sloc }
| "&"   { make_bin_bit_and $sloc }
| "|"   { make_bin_bit_or $sloc }
| "^"   { make_bin_bit_xor $sloc }
