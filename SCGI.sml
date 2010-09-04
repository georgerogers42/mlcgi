functor SCGI(structure M : ORD_MAP where type Key.ord_key = string;
             structure NS : FMT) :> CGI_TYPE 
        where type env    = string M.map
          and type post   = string
          and type stream = NS.stream = struct
  structure Stream = NS.Stream
  exception OddList
  type post = string
  type env = string M.map
  fun insert m (x::x'::xs) = insert (M.insert(m,x,x')) xs
    | insert m [x]         = raise(OddList)
    | insert m []          = m
  fun read_env stream = let
    val parse = NS.parse stream
  in
    (insert M.empty (String.fields (fn x => x= #"\000") parse))
  end
  fun post s = NS.Stream.inputAll(s)
  type stream = NS.stream
end
