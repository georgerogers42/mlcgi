structure EnvMap = RedBlackMapFn(struct
  type ord_key = string
  val compare  = String.compare
end)
structure SCGI_RedBlack = SCGI(EnvMap);
let
  val SOME str = TextIO.inputLine TextIO.stdIn
  val (m,x)    = SCGI_RedBlack.read_env str
in
  EnvMap.appi (fn (x,y) => print(x ^ " " ^ y ^ "\n")) m
end;
