(*
 * This file is part of Bolt.
 * Copyright (C) 2009-2012 Xavier Clerc.
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


let register_logger normalized_name level filter pass layout mode output (output_name, output_rotate) =
  let layout = lazy (try Layout.get layout with Not_found -> Layout.default) in
  let output = lazy
      (try
        (Output.get output) output_name output_rotate layout
      with Not_found -> Output.void output_name output_rotate layout) in
  let infos = { Tree.name = normalized_name;
                level; filter; pass; layout; mode; output; } in
  Tree.register_logger infos

let register name level filter pass layout mode output output_params =
  let normalized_name = Name.of_string name in
  let filter = lazy (try Filter.get filter with Not_found -> Filter.all) in
  let pass = lazy (try Filter.get pass with Not_found -> Filter.all) in
  register_logger normalized_name level filter pass layout mode output output_params

let log_event name level file line column properties error msg =
  let orig_event =
    Event.make
      name
      level
      ~file:file
      ~line:line
      ~column:column
      ~properties:properties
      ~error:error
      msg in
  let loggers = Tree.get_loggers name in
  try
    List.iter
      (fun (nam, lst) ->
	let event = Event.with_logger nam orig_event in
	List.iter
          (fun logger ->
            try
              let _, _, layout = Lazy.force logger.Tree.layout in
              let mode = logger.Tree.mode in
              let output = Lazy.force logger.Tree.output in
              let filter = Lazy.force logger.Tree.filter in
	      let pass = Lazy.force logger.Tree.pass in
              if (level <= logger.Tree.level) && (filter event) then
		mode#deliver output (layout event);
	      if not (pass event) then
		raise Exit
            with Exit -> raise Exit | _ -> ())
          lst)
      loggers
  with
    Exit -> ()

let check_level name level =
(*
  Printf.fprintf stderr "check_level: \"%s\" %s\n" name (Level.to_string level);
*)
  let name = Name.of_string name in
  let loggers = Tree.get_loggers name in
  try
    List.iter
      (fun (_, lst) ->
	if List.exists (fun logger -> level <= logger.Tree.level) lst then
	  raise Exit
      ) loggers;
    false
  with
    Exit -> true

let logf name level ?(file="") ?(line=(-1)) ?(column=(-1)) ?(properties=[]) ?(error=None) fmt =
  let f msg =
    let name = Name.of_string name in
    log_event name level file line column properties error msg
  in
  Printf.ksprintf f fmt

let log name level ?(file="") ?(line=(-1)) ?(column=(-1)) ?(properties=[]) ?(error=None) msg =
  let name = Name.of_string name in
  log_event name level file line column properties error msg

let prepare name =
  let name = Name.of_string name in
  Tree.make_node name

let configuration =
  let get_from_env () =
    try
      (Sys.getenv "BOLT_CONFIG"), ConfigurationNew.load
    with _ ->
      (Sys.getenv "BOLT_FILE"), ConfigurationOld.load in
  try
    let file, func = get_from_env () in
    Some (func file)
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

let () =
  try
    let plugins_env = Sys.getenv "BOLT_PLUGINS" in
    let plugins_list = Utils.split "," plugins_env in
    let plugins_list = List.map Utils.trim plugins_list in
    List.iter Dynlink.loadfile_private plugins_list
  with _ -> ()

let () =
  let used_signals = Array.make Signal.max_int false in
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
  let rec make_filter = function
    | Configuration.Identifier s
    | Configuration.String s ->
        lazy (try Filter.get s with Not_found -> Filter.all)
    | Configuration.Integer _
    | Configuration.Float _ ->
        lazy Filter.all
    | Configuration.And (v1, v2) ->
        let f1 = make_filter v1 in
        let f2 = make_filter v2 in
        lazy (fun e -> ((Lazy.force f1) e) && ((Lazy.force f2) e))
    | Configuration.Or (v1, v2) ->
        let f1 = make_filter v1 in
        let f2 = make_filter v2 in
        lazy (fun e -> ((Lazy.force f1) e) || ((Lazy.force f2) e)) in
  match configuration with
  | Some conf ->
    List.iter
      (fun section ->
        let assoc x = List.assoc x section.Configuration.elements in
        let assoc_string x =
          try
            match List.assoc x section.Configuration.elements with
            | Configuration.Identifier s
            | Configuration.String s -> Some s
            | _ -> None
          with _ -> None in
        let level =
          try
            match assoc_string "level" with
            | Some s -> Level.of_string s
            | None -> Level.FATAL
          with _ -> Level.FATAL in
        let filter =
          try
            match assoc "filter" with
            | Configuration.Identifier s
            | Configuration.String s ->
                lazy (try Filter.get s with Not_found -> Filter.all)
            | f -> make_filter f
          with _ -> lazy Filter.all in
        let pass =
          try
            match assoc "pass" with
            | Configuration.Identifier s
            | Configuration.String s ->
                lazy (try Filter.get s with Not_found -> Filter.all)
            | f -> make_filter f
          with _ -> lazy Filter.all in
        let layout =
          try
            match assoc "layout" with
            | Configuration.Identifier "pattern"
            | Configuration.String "pattern" ->
                let header =
                  try
                    match assoc_string "pattern-header-file" with
                    | Some s -> read_lines s
                    | None -> []
                  with _ -> [] in
                let footer =
                  try
                    match assoc_string "pattern-footer-file" with
                    | Some s -> read_lines s
                    | _ -> []
                  with _ -> [] in
                let pattern =
                  match assoc_string "pattern" with
                  | Some s -> s
                  | None -> "" in
                Layout.register_unnamed (Layout.pattern header footer pattern)
            | Configuration.Identifier "csv"
            | Configuration.String "csv" ->
                let sep =
                  match assoc_string "csv-separator" with
                  | Some s -> s
                  | None -> ";" in
                let list =
                  match assoc_string "csv-elements" with
                  | Some s -> s
                  | None -> "" in
                let elems = Utils.split " \t" list in
                Layout.register_unnamed (Layout.csv sep elems)
            | Configuration.Identifier s
            | Configuration.String s ->
                s
            | _ ->
                "default"
          with _ -> "default" in
        let mode =
          try
            match assoc "mode" with
            | Configuration.Identifier "direct"
            | Configuration.String "direct" ->
                Mode.direct ()
            | Configuration.Identifier "memory"
            | Configuration.String "memory" ->
                Mode.memory ()
            | Configuration.Identifier "retained"
            | Configuration.String "retained" ->
                Mode.retained
                  (match assoc_string "retention" with
                  | Some s -> s
                  | None -> "")
            | _ ->
                Mode.direct ()
          with _ -> Mode.direct () in
        let output =
          match assoc_string "output" with
          | Some s -> s
          | None -> "file" in
        let name =
          match assoc_string "name" with
          | Some s -> s
          | None -> "<stderr>" in
        let seconds =
          try
            match assoc "rotate" with
            | Configuration.Integer i ->
                Some (float_of_int i)
            | Configuration.Float f ->
                Some f
            | Configuration.Identifier s
            | Configuration.String s ->
                Some (float_of_string s)
            | _ ->
                None
          with _ -> None in
        let signal =
          try
            match assoc_string "signal" with
            | Some s ->
                let s = Signal.of_string s in
                used_signals.(Signal.to_int s) <- true;
                Some s
            | None -> None
          with _ -> None in
        let rotate = { Output.seconds_elapsed = seconds;
                       Output.signal_caught = signal; } in
        register_logger
          section.Configuration.name
          level
          filter
	  pass
          layout
          mode
          output
          (name, rotate))
      conf;
      Array.iteri
        (fun i v ->
          let handler _ =
            Output.signals.(i) <- true in
          let s = Signal.to_sys (Signal.of_int i) in
          if v then Sys.set_signal s (Sys.Signal_handle handler))
        used_signals
  | None ->
      ()
