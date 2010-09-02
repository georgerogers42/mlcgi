signature STR = sig
  val length : string -> int * int
  val parse : string -> string
end
structure NetString :> STR = struct
  exception NoColon
  fun ilength a (s,i) =
  let 
    val c = String.sub(s,i)
  in
    if Char.isDigit c then
      ilength (a*10+(ord c - ord #"0")) (s,i+1)
    else if c = #":" then
      (a,i)
    else
      raise NoColon

  end
  fun length s = ilength 0 (s,0)
  fun parse s =
  let
    val (len,chrs) = length s
    val str = String.explode s
  in
    String.implode(List.take(List.drop(str,chrs+1),len))
  end
end
