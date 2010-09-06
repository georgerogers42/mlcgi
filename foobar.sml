exception Failure of string
structure EnviromentMap = RedBlackMapFn(struct
  type ord_key = string
  val  compare = String.compare
end);
structure TheCgi = CGI(EnviromentMap)
structure ErrPages :> ERROR_PAGES = struct
  fun errorPage (n,Failure(s)) () = print("Status: " ^ Int.toString n ^
                                          "\nContent-type: text/plain\n\n" ^
                                           s ^ " " ^ (Int.toString n))
    | errorPage (n,e) () = print("Status: " ^ Int.toString n ^
                              "\nContent-type: text/plain\n\n" ^
                              (Int.toString n))
                                          
end
structure Baz :> APPLICATION = struct
  structure EnvMap = EnviromentMap
  structure ErrorPages = ErrPages
  structure Cgi = TheCgi
  fun fib (SOME n) = if n > 2 then fib(SOME(n-1))+fib(SOME(n-2)) else n
    | fib (NONE)   = raise Failure("Ilegal Fib")
  fun foo [x] = SOME(fn () => print("Content-type: text/plain\n\n" ^ x))
    | foo []  = SOME(fn () => print("Content-type: text/plain\n\n" ^ "foo"))
    | foo ["fib",n] = SOME(fn () => print("Content-type: text/plain\n\n" ^
                                          Int.toString(fib(Int.fromString n))))
    | foo _   = NONE
  fun bar [x,y] = SOME(fn () => print("Content-type: text/plain\n\n" ^x^y))
    | bar _     = NONE
  val dispatch = foo o' bar
end
structure TheApp = App(Baz)
val _ = TheApp.dispatch()
