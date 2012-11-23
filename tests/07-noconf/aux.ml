let f1 x =
  LOG "entering 'f1'" LEVEL INFO;
  print_endline x;
  LOG "leaving 'f1'" LEVEL INFO

let f2 n x =
  LOG "entering 'f2'" LEVEL INFO;
  if (n < 0) then LOG "negative n" LEVEL WARN;
  for i = 1 to n do
    LOG "inside 'f2' loop" LEVEL TRACE;
    f1 x
  done;
  LOG "leaving 'f2'" LEVEL INFO
