let
  val SOME str = TextIO.inputLine TextIO.stdIn
in
  print(NetString.parse str ^ "\n")
end;
