let () =
  Bolt.Filter.register
    "myfilter"
    (fun e -> (e.Bolt.Event.line mod 2) = 0)

let () =
  Bolt.Layout.register
    "mylayout"
    ([],
     [],
     (fun e ->
       Printf.sprintf "file \"%s\" says \"%s\" with level \"%s\" (line: %d)"
         e.Bolt.Event.file
         e.Bolt.Event.message
         (Bolt.Level.to_string e.Bolt.Event.level)
         e.Bolt.Event.line))
