module Definitions = struct
  let logger = ""
  let level = Bolt.Level.TRACE

  type container_type = [ `Program ]
  let container_types = [ `Program, "Program", None, "P" ]

  type event_type = [ `Start | `End ]
  let event_types = [ `Start, "Start", `Program, "S" ;
                      `End, "End", `Program, "E" ]

  type state_type = [ `Running ]
  let state_types = [ `Running, "Running", `Program, "R" ]

  type variable_type = unit
  let variable_types = []

  type link_type = [ `Message ]
  let link_types = [ `Message, "Message", `Program, `Program, `Program, "M" ]

  type entity_value_type = unit
  let entity_value_types = [ (), "", `Program, (1.0, 1.0, 1.0), "" ]
end

module MyPaje = Bolt.Paje.Make (Definitions)

let sender ch =
  for i = 0 to 9 do
    Thread.delay (0.1 +. (Random.float 0.2));
    LOG MyPaje.t
      WITH MyPaje.start_link ~typ:`Message ~container:"main" ~start_container:"sender" ~value:(string_of_int i) ~key:"i" []
      LEVEL TRACE;
    Event.sync (Event.send ch i);
  done;
  Thread.delay (0.1 +. (Random.float 0.2));
  LOG MyPaje.t
    WITH MyPaje.start_link ~typ:`Message ~container:"main" ~start_container:"sender" ~value:"99" ~key:"i" []
    LEVEL TRACE;
  Event.sync (Event.send ch 99)

let receiver ch =
  let last = ref 0 in
  while !last <> 99 do
    let e = Event.receive ch in
    last := Event.sync e;
    LOG MyPaje.t
      WITH MyPaje.end_link ~typ:`Message ~container:"main" ~end_container:"receiver" ~value:(string_of_int !last) ~key:"i" []
      LEVEL TRACE;
    print_int !last;
    print_newline ()
  done

let _ =
  LOG MyPaje.t
    WITH MyPaje.create_container ~name:"main" ~typ:`Program []
    LEVEL TRACE;
  LOG MyPaje.t
    WITH MyPaje.create_container ~name:"sender" ~typ:`Program ~container:"main" []
    LEVEL TRACE;
  LOG MyPaje.t
    WITH MyPaje.create_container ~name:"receiver" ~typ:`Program ~container:"main" []
    LEVEL TRACE;
  LOG MyPaje.t
    WITH MyPaje.new_event ~typ:`Start ~container:"main" ~value:"" []
    LEVEL TRACE;
  Random.self_init ();
  let ch = Event.new_channel () in
  let s = Thread.create sender ch in
  let r = Thread.create receiver ch in
  Thread.join s;
  Thread.join r;
  LOG MyPaje.t
    WITH MyPaje.new_event ~typ:`End ~container:"main" ~value:"" []
    LEVEL TRACE





(*
type r = { x : int; y : float }
let r =
  let open Bolt.Daikon in
  make_variable_builder
    (fun r -> [int "x" r.x; float "y" r.y])
let make_r k =
  { x = k * k; y = float (k - 2); }

type c = bool * string
let c =
  let open Bolt.Daikon in
  tuple2 bool string
let make_c k =
  true, String.make k '*'
  

let f x =
  LOG Daikon.t
    WITH Daikon.enter "f" [Daikon.int "x" x] LEVEL TRACE;
  let rr = make_r x in
  let cc = make_c x in
  let res = (x * x) mod 2 in
  LOG Daikon.t
    WITH Daikon.exit "f" (Daikon.int "res" res) [Daikon.int "x" x; r "rr" rr; c "cc" cc]
    LEVEL TRACE

let () =
  let l = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  List.iter f l
*)
