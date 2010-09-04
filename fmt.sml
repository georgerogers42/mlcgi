signature FMT = sig
  structure Stream : TEXT_IO
  type stream
  val length : Stream.instream -> int
  val parse : Stream.instream -> string
  sharing type stream = Stream.instream
end
