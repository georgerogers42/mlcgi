functor SCGI(M : ORD_MAP where type Key.ord_key = string) :> CGI_TYPE 
        where type stream = string
          and type env    = string M.map
          and type post   = string option = struct
  exception OddList
  type stream = string
  type env = string M.map
  type post = string option
  fun insert m (x::x'::xs) = insert (M.insert(m,x,x')) xs
    | insert m [x]         = (m,SOME x)
    | insert m []          = (m,NONE)
  fun read_env stream = insert M.empty (String.fields (fn x => x= #"\000") (NetString.parse stream))
end
