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

