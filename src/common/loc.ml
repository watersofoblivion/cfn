type pos = {
  line: int;
  col:  int;
  off:  int
}

let pos pos = {
  line = pos.Lexing.pos_lnum;
  col  = pos.pos_cnum - pos.pos_bol;
  off  = pos.pos_cnum
}

type t = {
  start_pos : pos;
  end_pos   : pos;
}

let loc start_pos end_pos =
  { start_pos = pos start_pos;
    end_pos   = pos end_pos }
