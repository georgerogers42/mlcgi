functor NetString(T : TEXT_IO) :> FMT where type stream = T.instream = struct
  exception NoColon
  exception NoComma
  structure Stream = T
  fun ilength a (s,i) =
  let
    val c = valOf(T.input1(s))
  in
    if Char.isDigit c then
      ilength (a*10+(ord c - ord #"0")) (s,i+1)
    else if c = #":" then
       a
    else
      raise NoColon
  end
  fun length s = ilength 0 (s,0)
  fun parse s = let
    val len = length s
    val parsed = T.inputN(s,len)
    val sfx = valOf(T.input1(s))
  in
    if sfx = #"," then
      parsed
    else
      raise NoComma
  end
  type stream = T.instream
end
