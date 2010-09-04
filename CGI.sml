signature CGI_TYPE = sig
  type env
  val readParams : unit -> env
  val urlDecode  : string -> string
  val urlEncode  : string -> string
  val pathInfo   : unit   -> string list
end
functor CGI(M : ORD_MAP where type Key.ord_key = string) :> CGI_TYPE
        where type env = string M.map = struct
  exception Pair
  type env = string M.map
  fun urlEncodeInternal a s = let
    val ss   = Substring.getc s
  in
    case ss of
         SOME (x,xs) =>
            if Char.isAlphaNum x orelse x = #"-"
            orelse x = #"_" orelse x = #"." orelse x = #"~"
            then
              urlEncodeInternal (a ^ String.str x) xs
            else
              urlEncodeInternal (a ^ "%" ^ Int.fmt StringCvt.HEX(ord x)) xs
       | NONE =>
            a
  end
  fun urlDecodeInternal a (s : Substring.substring) = let
    val ss   = Substring.getc s
  in
    case ss of
         SOME (x,xs) =>
             if x = #"%" then
               let
                 val xxs = Substring.string xs
                 val c = valOf(StringCvt.scanString (Int.scan (StringCvt.HEX))
                                                    (String.substring(xxs,0,2)))
               in
                 urlDecodeInternal (a^String.str(chr c))
                                   (Substring.extract(xxs,2,NONE))
               end
             else if x = #"+" then
               urlDecodeInternal (a^" ") xs
             else
               urlDecodeInternal (a^String.str x) xs
       | NONE =>
             a
  end
  fun urlEncode s = 
    urlEncodeInternal "" (Substring.full s)
  fun urlDecode s = 
    urlDecodeInternal "" (Substring.full s)
  fun pair [x,y] = (x,y)
    | pair x     = raise(Pair)
  fun insert m s = let
    val (x,y) = pair(List.map urlDecode (String.fields (fn c => c = #"=") s))
  in
    M.insert(m,x,y)
  end
  fun insertAll m (x::xs) = insertAll (insert m x) xs
    | insertAll m []      = m
  fun tokenize s = String.tokens (fn c => c = #";" orelse c = #"&") s
  fun readGetParams()  =
    insertAll (M.empty)
              (tokenize (valOf (OS.Process.getEnv "QUERY_STRING")))
  fun readPostParams() = let
    val clength = valOf(Int.fromString(valOf(OS.Process.getEnv "CONTENT_LENGTH")))
  in
    insertAll (M.empty) (tokenize (TextIO.inputN(TextIO.stdIn,clength)))
  end
  fun readParams() =
    if valOf(OS.Process.getEnv "REQUEST_METHOD") = "GET" then
      readGetParams()
    else
      readPostParams()
  fun pathInfo () = 
    case OS.Process.getEnv "PATH_INFO" of
         SOME s => String.tokens (fn c => c = #"/") s
       | NONE => []
end
