infix 3 o'
fun x o' y = fn z =>
  case x z of
       SOME a => SOME a
     | NONE   => y z
signature ERROR_PAGES = sig
  val errorPage : int * exn -> unit -> unit
end
structure Page = struct
  type dispatch = string list -> (unit -> unit) option
end
signature APPLICATION = sig
  structure EnvMap : ORD_MAP where type Key.ord_key = string
  structure Cgi : CGI_TYPE
  structure ErrorPages : ERROR_PAGES
  val dispatch : Page.dispatch
end
signature APP = sig
  val dispatch : unit -> unit
end
functor App(A : APPLICATION) = struct
  fun dispatch () = 
    (case A.dispatch(A.Cgi.pathInfo()) of
             SOME f => f()
           | NONE   => A.ErrorPages.errorPage(404,Option) ())
    handle x => A.ErrorPages.errorPage(500,x) ()
end
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
signature FMT = sig
  structure Stream : TEXT_IO
  type stream
  val length : Stream.instream -> int
  val parse : Stream.instream -> string
  sharing type stream = Stream.instream
end
structure EnviromentMap = RedBlackMapFn(struct
  type ord_key = string
  val  compare = String.compare
end);
structure TheCgi = CGI(EnviromentMap)
structure ErrPages :> ERROR_PAGES = struct
  fun errorPage (n,x) () = print("Status: " ^ Int.toString n ^
                                 "\nContent-type: text/html\n\n" ^ Int.toString n)
end
structure Baz :> APPLICATION = struct
  structure EnvMap = EnviromentMap
  structure ErrorPages = ErrPages
  structure Cgi = TheCgi
  fun foo [x] = SOME(fn () => print("Content-type: text/plain\n\n" ^ x))
    | foo []  = SOME(fn () => print("Content-type: text/plain\n\n" ^ "foo"))
    | foo [x,"crash"] = SOME(fn () => raise Option)
    | foo _   = NONE
  fun bar [x,y] = SOME(fn () => print("Content-type: text/plain\n\n" ^x^y))
    | bar _     = NONE
  val dispatch = foo o' bar
end
structure TheApp = App(Baz)
val _ = TheApp.dispatch()
signature XML = sig
  datatype extern_xml = TAG of {tag : string,args : (string * string) list,
                                body : extern_xml list}
                      | TXT of string
  type xml
  (* render xml nodes escapeing TXT nodes *)
  val render : xml -> string
  val extern : extern_xml -> xml
  val intern : xml -> extern_xml
end
structure Xml :> XML = struct
  datatype extern_xml = TAG of {tag : string,args : (string * string) list,
                                body : extern_xml list}
                      | TXT of string
  type xml = extern_xml
  val renderArgs = String.concatWith " " o List.map (fn (x,y) => x^"="^y)
  fun escape x = x
  fun render (TAG {tag = t,args = a,body=x}) = "<"^t^renderArgs a^">"^
                                               renderList x ^ "</"^t^">"
    | render (TXT x) = escape x
  and renderList x = (String.concat o List.map render) x
  fun extern x = x  
  fun intern x = x  
end
