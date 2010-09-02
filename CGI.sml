signature CGI_TYPE = sig
  type stream
  type post
  type env
  val read_env : stream -> env * post
end
