let main () =
  let len = Array.length Sys.argv in
  for i = 1 to pred len do
    Aux.f2 len Sys.argv.(i)
  done

let () =
  LOG "start ..." LEVEL INFO;
  (try 
    main ()
  with e ->
    LOG "uncaught exception" EXCEPTION e LEVEL FATAL;
    Printexc.print_backtrace stdout);
  LOG "end ..." LEVEL INFO
