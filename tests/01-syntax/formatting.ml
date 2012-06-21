let () =
  LOG "no parameter" LEVEL FATAL;
  LOG "one parameter: %d" 1 LEVEL INFO;
  LOG "two parameters: %d %s" 1 "abc" LEVEL ERROR;
  LOG "three parameters: %d %s %f" 1 "abc" 3. LEVEL DEBUG;
  LOG "four parameters: %d %s %f %B" 1 "abc" 3. true LEVEL WARN;
  LOG "five parameters: %d %s %f %B %c" 1 "abc" 3. true 'x' LEVEL TRACE;
  ()

let id x = x

let () =
  LOG "no parameter" LEVEL FATAL;
  LOG "one parameter: %d" (id 1) LEVEL INFO;
  LOG "two parameters: %d %s" 1 (id "abc") LEVEL ERROR;
  LOG "three parameters: %d %s %f" 1 "abc" (id 3.) LEVEL DEBUG;
  LOG "four parameters: %d %s %f %B" 1 "abc" 3. (id true) LEVEL WARN;
  LOG "five parameters: %d %s %f %B %c" 1 "abc" 3. true (id 'x') LEVEL TRACE;
  ()
