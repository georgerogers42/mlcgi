structure NetString :> FMT = struct
  exception NoColon
  exception NoComma
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
  fun parse s = let
    val (len,chrs) = length s
    val parsed = String.substring(s,chrs+1,len)
    val sfx = String.sub(s,len+chrs+1)
  in
    if sfx = #"," then
      parsed
    else
      raise NoComma
  end
end
