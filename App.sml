infix 3 o'
fun x o' y = fn z =>
  case x z of
       SOME a => SOME a
     | NONE   => y z
signature ERROR_PAGES = sig
  val errorPage : int -> unit -> unit
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
  fun dispatch () = case A.dispatch(A.Cgi.pathInfo()) of
                         SOME f => f()
                       | NONE   => A.ErrorPages.errorPage 404 ()
end
