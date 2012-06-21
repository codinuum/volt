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


(* Definitions *)

class type impl =
  object
    method write : string -> unit
    method close : unit
  end

type rotation = {
    seconds_elapsed : float option;
    signal_caught   : int option;
  }

type t = string -> rotation -> Layout.t lazy_t -> impl

let outputs, register, register_unnamed, get =
  Utils.make_container_functions ()


(* Predefined outputs *)

let void _ _ _ =
  object
    method write _ = ()
    method close = ()
  end

let outputname s t =
  let dir = Filename.dirname s in
  let base = Filename.basename s in
  let buff_sz = 64 + (String.length s) in
  let buff = Buffer.create buff_sz in
  let time =
    let now = Unix.localtime t in
    let millis = int_of_float ((t -. (floor t)) *. 1000.) in
    Printf.sprintf "%d-%02d-%02d-%02d-%02d-%02d-%03d"
      (1900 + now.Unix.tm_year)
      (succ now.Unix.tm_mon)
      now.Unix.tm_mday
      now.Unix.tm_hour
      now.Unix.tm_min
      now.Unix.tm_sec
      millis in
  String.iter
    (function
      | '%' -> Buffer.add_string buff time
      | c -> Buffer.add_char buff c)
    base;
  let file =
    try
      let file = Buffer.create buff_sz in
      let try_with_empty f x =
        try f x with _ -> "" in
      let subst = function
        | "time" -> time
        | "pid" -> try_with_empty (fun () -> string_of_int (Unix.getpid ())) ()
        | "hostname" -> try_with_empty Unix.gethostname ()
        | s -> try_with_empty Sys.getenv s in
      Buffer.add_substitute file subst (Buffer.contents buff);
      Buffer.contents file
    with Not_found -> Buffer.contents buff in
  Filename.concat dir file

let write_strings ch l =
  try
    List.iter (fun s -> output_string ch s; output_char ch '\n') l
  with _ ->
    Utils.verbose "unable to write data"

let open_channel filename h t =
  let ch, reg = match filename with
  | "<stdout>" -> stdout, false
  | "<stderr>" -> stderr, false
  | _ ->
      let file = outputname filename t in
      let kind = try (Unix.stat file).Unix.st_kind with _ -> Unix.S_REG in
      (open_out file),
      (match kind with Unix.S_REG | Unix.S_LNK -> true | _ -> false) in
  write_strings ch h;
  (try flush ch with _ -> Utils.verbose "unable to write data");
  ch, reg

let signal = ref false

let file filename rot layout =
  let now = Unix.gettimeofday () in
  let header, footer, _ = Lazy.force layout in
  try
    let ch, regular = open_channel filename header now in
    let rotate = if regular then Some rot else None in
    object (self)
      val mutable channel = ch
      val mutable last_rotate = now
      method write s =
        try
          if s <> "" then begin
            output_string channel s;
            output_char channel '\n';
            flush channel;
          end;
          let now = Unix.gettimeofday () in
          match rotate with
          | Some r ->
              let do_rotate =
                (match r.seconds_elapsed with
                | Some x ->
                    now -. last_rotate >= x
                | None -> false)
              || (match r.signal_caught with
                | Some _ ->
                    let res = !signal in
                    signal := false;
                    res
                | None -> false) in
              if do_rotate then begin
                self#close;
                channel <- fst (open_channel filename header now);
                last_rotate <- now
              end
          | None -> ()
        with _ -> Utils.verbose "unable to write data"
      method close =
        write_strings ch footer;
        close_out_noerr ch
    end
  with _ ->
    Utils.verbose "unable to create output (resorting to 'void')";
    void filename rot layout

let growlnotify _ _ _ =
  object
    method write msg =
      try
        let progname = Sys.argv.(0) in
        let basename = Filename.basename progname in
        let command = Printf.sprintf "growlnotify -n %s -t %s -m %s"
            (Filename.quote progname)
            (Filename.quote basename)
            (Filename.quote msg) in
        ignore (Sys.command command)
      with _ -> ()
    method close = ()
  end

let () =
  List.iter
    (fun (x, y) -> register x y)
    [ "void",        void ;
      "file",        file ;
      "growlnotify", growlnotify ]
