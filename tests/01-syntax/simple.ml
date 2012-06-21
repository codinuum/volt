let main () =
  LOG "entering main" LEVEL TRACE;
  if (Array.length Sys.argv) = 0 then
    LOG "no %s" "argument" LEVEL WARN;
  for i = 1 to pred (Array.length Sys.argv) do
    try
      LOG "getting variable " LEVEL DEBUG;
      print_endline (Sys.getenv Sys.argv.(i))
    with
    | Not_found ->
        LOG "undefined variable" PROPERTIES ["var", Sys.argv.(i)] LEVEL ERROR
  done;
  LOG "leaving main" LEVEL TRACE

let () =
  LOG "start ..." NAME "App" LEVEL INFO;
  (try 
    main ()
  with e ->
    LOG "uncaught exception" EXCEPTION e LEVEL FATAL;
    Printexc.print_backtrace stdout);
  LOG "end ..." NAME "App" LEVEL INFO
