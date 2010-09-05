signature XML = sig
  datatype extern_xml = TAG of {tag : string,args : (string * string) list,
                                body : extern_xml list}
                      | TXT of string
  type xml
  (* render xml nodes escapeing TXT nodes *)
  val render : xml -> string
  val extern : extern_xml -> xml
  val intern : xml -> extern_xml
end
structure Xml :> XML = struct
  datatype extern_xml = TAG of {tag : string,args : (string * string) list,
                                body : extern_xml list}
                      | TXT of string
  type xml = extern_xml
  val renderArgs = String.concatWith " " o List.map (fn (x,y) => x^"="^y)
  fun escape x = x
  fun render (TAG {tag = t,args = a,body=x}) = "<"^t^renderArgs a^">"^
                                               renderList x ^ "</"^t^">"
    | render (TXT x) = escape x
  and renderList x = (String.concat o List.map render) x
  fun extern x = x  
  fun intern x = x  
end
