signature ERROR_PAGES = sig
  val errorPage : int -> unit -> unit
end
signature APPLICATION = sig
  structure EnvMap : ORD_MAP where type Key.ord_key = string
  structure Cgi : CGI_TYPE
  structure ErrorPages : ERROR_PAGES
  type dispatch = string list -> (unit -> unit) option
  val dispatch : dispatch
end
signature APP = sig
  val dispatch : unit -> unit
end
functor App(A : APPLICATION) =
struct
  fun dispatch () = case A.dispatch(A.Cgi.pathInfo()) of
                         SOME f => f()
                       | NONE   => A.ErrorPages.errorPage 500 ()
end
