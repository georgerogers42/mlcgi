structure Application :> APPLICATION = struct
  structure EnvMap = RedBlackMapFn(struct
    type ord_key = string
    val  compare = String.compare
  end);
  structure Cgi = CGI(EnvMap);
  structure ErrorPages :> ERROR_PAGES = struct
    fun errorPage n   () = print("Content-type: text/html\n\n" ^ Int.toString n)
  end
  type dispatch = string -> (unit -> unit) option
  fun renderSlash () = print("Content-type: text/html\n\n" ^ "xyz")
  fun dispatch "/" = SOME(renderSlash)
    | dispatch x  = SOME(ErrorPages.errorPage 404)
end
structure TheApp = App(Application)
val _ = TheApp.dispatch()
