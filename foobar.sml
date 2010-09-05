structure EnviromentMap = RedBlackMapFn(struct
  type ord_key = string
  val  compare = String.compare
end);
structure TheCgi = CGI(EnviromentMap)
structure ErrPages :> ERROR_PAGES = struct
  fun errorPage n   () = print("Status: " ^ Int.toString n ^
                               "\nContent-type: text/html\n\n" ^ Int.toString n)
end
structure Foo :> PAGE = struct
  type dispatch = string list -> (unit -> unit) option
  fun index ()  = print("Content=type: text/html\n\n" ^ "Hello World")
  fun dispatch [] = SOME(index)
    | dispatch _  = NONE
end
structure Bar :> PAGE = struct
  type dispatch = string list -> (unit -> unit) option
  fun foo x () = print("Content-type: text/html\n\n" ^ x)
  fun dispatch ["foo",x] = SOME(foo x)
    | dispatch _         = NONE
end
structure Baz :> APPLICATION = struct
  structure EnvMap = EnviromentMap
  structure ErrorPages = ErrPages
  structure Cgi = TheCgi
  structure Page = Compose(struct
    structure X = Foo
    structure Y = Bar
  end)
end
structure TheApp = App(Baz)
val _ = TheApp.dispatch()
