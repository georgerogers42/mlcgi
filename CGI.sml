structure EnvMap = BinaryMapFn(struct
  type ord_key = String.string
  val compare  = String.compare
end)
signature CGI_TYPE = sig
  type stream
  datatype env = CGI of {
        env  : string EnvMap.map,
        post : string option
  }
  val read_env : stream -> env
end
