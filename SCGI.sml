structure SCGI :> CGI_TYPE where type stream = string = struct
  type stream = string
  datatype env = CGI of {
        env : string EnvMap.map,
        post : string option
  }
  fun read_env stream = CGI { env = EnvMap.empty,post = NONE }
end
