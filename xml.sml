signature XML = sig
  datatype xml = TAG of {tag : string,args : (string * string) list,
                                body : xml list}
               | TXT of string
  (* render xml nodes escapeing TXT nodes *)
  val render : xml -> string
end
structure Xml :> XML = struct
  datatype extern_xml = TAG of {tag : string,args : (string * string) list,
                                body : extern_xml list}
                      | TXT of string
  datatype xml = datatype extern_xml
  val renderArgs = String.concatWith " " o List.map (fn (x,y) => x^"="^y)
  fun escape x = x
  fun render (TAG {tag = t,args = a,body=x}) = "<"^t^renderArgs a^">"^
                                               renderList x ^ "</"^t^">"
    | render (TXT x) = escape x
  and renderList x = (String.concat o List.map render) x
end
