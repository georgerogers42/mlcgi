infix 3 o'
fun x o' y = fn z =>
  case x z of
       SOME a => SOME a
     | NONE   => y z
signature ERROR_PAGES = sig
  val errorPage : int -> unit -> unit
end
signature PAGE = sig
  type dispatch = string list -> (unit -> unit) option
  val dispatch : dispatch
end
signature APPLICATION = sig
  structure EnvMap : ORD_MAP where type Key.ord_key = string
  structure Cgi : CGI_TYPE
  structure ErrorPages : ERROR_PAGES
  structure Page : PAGE
end
signature PAGE_COMP = sig
  structure X : PAGE
  structure Y : PAGE
end
functor Compose(X : PAGE_COMP) :> PAGE = struct
  type dispatch = string list -> (unit -> unit) option
  val dispatch  = X.X.dispatch o' X.Y.dispatch
end
signature APP = sig
  val dispatch : unit -> unit
end
functor App(A : APPLICATION) =
struct
  fun dispatch () = case A.Page.dispatch(A.Cgi.pathInfo()) of
                         SOME f => f()
                       | NONE   => A.ErrorPages.errorPage 404 ()
end
