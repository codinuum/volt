(*
 * This file is part of Bolt.
 * Copyright (C) 2009-2011 Xavier Clerc.
 *
 * Bolt is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Bolt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


type t = {
    name   : string list;
    level  : Level.t;
    filter : Filter.t lazy_t;
    pass   : Filter.t lazy_t;
    layout : Layout.t lazy_t;
    output : Output.impl lazy_t;
  }

let names = Hashtbl.create 17

let normalize_name n =
  try
    Hashtbl.find names n
  with Not_found ->
    let res = Configuration.normalize n in
    Hashtbl.replace names n res;
    res

let loggers = Hashtbl.create 17

let close_all () =
  Hashtbl.iter
    (fun _ l ->
      List.iter (fun x -> (Lazy.force x.output)#close) l)
    loggers

let () = at_exit close_all

let register_logger name level filter pass layout output (output_name, output_rotate) =
  let layout = lazy (try Layout.get layout with Not_found -> Layout.default) in
  let pass = lazy (try Filter.get pass with Not_found -> Filter.all) in
  let filter = lazy (try Filter.get filter with Not_found -> Filter.all) in
  let output = lazy
      (try
        (Output.get output) output_name output_rotate layout
      with Not_found -> Output.void output_name output_rotate layout) in
  let logger = { name; level; filter; pass; layout; output; } in
  let l = try Hashtbl.find loggers name with Not_found -> [] in
  Hashtbl.replace loggers name (logger :: l)

let register name level filter pass layout output output_params =
  register_logger (normalize_name name) level filter pass layout output output_params

let remove_last l =
  let rec doit acc = function
    | [] -> assert false
    | _ :: [] -> List.rev acc
    | hd :: tl -> doit (hd :: acc) tl 
  in
  doit [] l

let log_event name norm_name level file line column properties error msg =
  let orig_event = 
    Event.make name level ~file:file ~line:line ~column:column ~properties:properties ~error:error msg 
  in
  let get n = try Hashtbl.find loggers n with Not_found -> [] in
  let loggers = ref (get norm_name) in
  let name = ref norm_name in
  try
    while !name <> [] do
      let event = Event.with_logger (String.concat "." !name) orig_event in
      List.iter
	(fun logger ->
          try
            let _, _, layout = Lazy.force logger.layout in
            let output = Lazy.force logger.output in
            let filter = Lazy.force logger.filter in
	    let pass = Lazy.force logger.pass in
            if (level <= logger.level) && (filter event) then
              output#write (layout event);
	    if not (pass event) then
	      raise Exit
          with
	  | Exit -> raise Exit
	  | _ -> ()
	) !loggers;
      match !name with
      | [""] ->
          name := [];
          loggers := []
      | _ ->
          name := remove_last !name;
          name := if !name = [] then [""] else !name;
          loggers := get !name
    done
  with
    Exit -> ()

let check_level name level =
(*
  Printf.fprintf stderr "check_level: \"%s\" %s\n" name (Level.to_string level);
*)
  let norm_name = normalize_name name in
  let get n = try Hashtbl.find loggers n with Not_found -> [] in
  let loggers = ref (get norm_name) in
  let name = ref norm_name in
  try
    while !name <> [] do
      if List.exists (fun logger -> level <= logger.level) !loggers then
	raise Exit;
      match !name with
      | [""] -> name := []; loggers := []
      | _ ->
	  name := remove_last !name;
	  if !name = [] then name := [""];
	  loggers := get !name
    done;
    false
  with
    Exit -> true


let logf name level ?(file="") ?(line=(-1)) ?(column=(-1)) ?(properties=[]) ?(error=None) fmt =
  let f msg =
    Utils.enter_critical_section ();
    begin
      try
	log_event name (normalize_name name) level file line column properties error msg
      with 
	e ->
	  Utils.leave_critical_section ();
	  raise e
    end;
    Utils.leave_critical_section ()
  in
  Printf.ksprintf f fmt

let log name level ?(file="") ?(line=(-1)) ?(column=(-1)) ?(properties=[]) ?(error=None) msg =
  Utils.enter_critical_section ();
  (try
    log_event name (normalize_name name) level file line column properties error msg
  with e ->
    Utils.leave_critical_section ();
    raise e);
  Utils.leave_critical_section ()

let configuration =
  try
    let file = Sys.getenv "BOLT_FILE" in
    Some (Configuration.load file)
  with
  | Not_found -> None
  | Configuration.Exception (line, err) ->
      let msg =
        Printf.sprintf "syntax error in configuration file at line %d:\n  %s"
          line
          err in
      Utils.verbose msg;
      None
  | _ ->
      Utils.verbose "unable to load configuration";
      None

let comma = Str.regexp "[ \t]+,[ \t]+"

let () =
  try
    let plugins_env = Sys.getenv "BOLT_PLUGINS" in
    let plugins_list = Str.split comma plugins_env in
    List.iter Dynlink.loadfile_private plugins_list
  with _ -> ()

let white_spaces = Str.regexp "[ \t]+"

let signal_of_string s =
  match String.lowercase s with
  | "sighup" -> Sys.sighup
  | "sigusr1" -> Sys.sigusr1
  | "sigusr2" -> Sys.sigusr2
  | _ -> failwith "unsupported signal"

let () =
  let read_lines file =
    try
      let ch = open_in file in
      let res = ref [] in
      (try
        while true do
          res := (input_line ch) :: !res
        done
      with End_of_file -> ());
      List.rev !res
    with _ -> [] in
  match configuration with
  | Some conf ->
    List.iter
      (fun section ->
        let assoc x = List.assoc x section.Configuration.elements in
        let level = try Level.of_string (assoc "level") with _ -> Level.FATAL in
        let filter = try assoc "filter" with _ -> "all" in
        let pass = try assoc "pass" with _ -> "all" in
        let layout =
          try
            match assoc "layout" with
            | "pattern" ->
                let header = try read_lines (assoc "pattern-header-file") with _ -> [] in
                let footer = try read_lines (assoc "pattern-footer-file") with _ -> [] in
                Layout.register_unnamed (Layout.pattern header footer (assoc "pattern"))
            | "csv" ->
                let sep = try assoc "csv-separator" with _ -> ";" in
                let elems = Str.split white_spaces (assoc "csv-elements") in
                Layout.register_unnamed (Layout.csv sep elems)
            | x -> x
          with _ -> "default" in
        let output = try assoc "output" with _ -> "file" in
        let name = try assoc "name" with _ -> "<stderr>" in
        let seconds = try Some (float_of_string (assoc "rotate")) with _ -> None in
        let signal = try Some (signal_of_string (assoc "signal")) with _ -> None in
        (match signal with
        | Some s ->
            let f _ =
              Output.signal := true in
            Sys.set_signal s (Sys.Signal_handle f)
        | None -> ());
        let rotate = { Output.seconds_elapsed = seconds;
                       Output.signal_caught = signal; } in
        register_logger section.Configuration.name level filter pass layout output (name, rotate))
      conf
  | None ->
      ()
