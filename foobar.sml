structure Application :> APPLICATION = struct
  structure EnvMap = RedBlackMapFn(struct
    type ord_key = string
    val  compare = String.compare
  end);
  structure Cgi = CGI(EnvMap);
  structure ErrorPages :> ERROR_PAGES = struct
    fun errorPage 404 () = print("Status: 404 NotFound\nContent-type: text/html\n\n" ^ Int.toString 404) 
      | errorPage n   () = print("Status: " ^ Int.toString n ^
                                 "\nContent-type:text/html\n\n" ^ Int.toString n)
  end
  type dispatch = string list -> (unit -> unit) option
  fun renderSlash () = print("Content-type: text/html\n\n" ^ "xyz")
  fun dispatch [] = SOME(renderSlash)
    | dispatch x  = SOME(ErrorPages.errorPage 404)
end
structure TheApp = App(Application)
val _ = TheApp.dispatch()
