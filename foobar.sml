structure EnviromentMap = RedBlackMapFn(struct
  type ord_key = string
  val  compare = String.compare
end);
structure TheCgi = CGI(EnviromentMap)
structure ErrPages :> ERROR_PAGES = struct
  fun errorPage (n,x) () = print("Status: " ^ Int.toString n ^
                                 "\nContent-type: text/html\n\n" ^
                                 Xml.render(Xml.TAG {tag = "a",args=[],body=[Xml.TXT(Int.toString n)]}))
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
