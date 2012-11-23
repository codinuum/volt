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


type time = float

let current_time () = Unix.gettimeofday ()

let initial_time = current_time ()

let time x = int_of_float (x *. 1000.)

let time64 x = Int64.of_float (x *. 1000.)

let next_id =
  let id = ref 0 in
  fun () ->
    incr id;
    !id

type t = {
    id : int;
    hostname : string;
    process : int;
    thread : int;
    timestamp : time;
    relative : int;
    level : Level.t;
    logger : Name.t;
    origin : Name.t;
    file : string;
    line : int;
    column : int;
    message : string;
    properties : (string * string) list;
    error : (exn * string) option;
  }

let make logger level ?(origin = None) ?(file = "") ?(line = ~-1)
    ?(column = ~-1) ?(properties = []) ?(error = None) message =
  let now = current_time () in
  let origin = match origin with
  | Some x -> x
  | None -> logger in
  let error = match error with
  | Some x -> Some (x, Printexc.get_backtrace ())
  | None -> None in
  { id = next_id ();
    hostname = (try Unix.gethostname () with _ -> "");
    process = (try Unix.getpid () with _ -> 0);
    thread = Utils.get_thread_id ();
    timestamp = now;
    relative = time (now -. initial_time);
    level = level;
    logger = logger;
    origin = origin;
    file = String.copy file;
    line = line;
    column = column;
    message = String.copy message;
    properties = properties;
    error = error }

let with_logger l e =
  { e with logger = l }

let string_of_wday = function
  | 0 -> "Sunday"
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | _ -> assert false

let string_of_month = function
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ -> assert false

let string_of_month_abrv m =
  (String.sub (string_of_month m) 0 3)

let bindings e =
  let string_of_properties l =
    "[" ^ (String.concat "; " (List.map (fun (x, y) -> x ^ ": " ^ y) l)) ^ "]" in
  let tm = Unix.localtime e.timestamp in
  let string_of_int' = function
    | 0 -> "00"
    | 1 -> "01"
    | 2 -> "02"
    | 3 -> "03"
    | 4 -> "04"
    | 5 -> "05"
    | 6 -> "06"
    | 7 -> "07"
    | 8 -> "08"
    | 9 -> "09"
    | x -> string_of_int x in
  [ "id",         string_of_int e.id;
    "hostname",   e.hostname;
    "process",    string_of_int e.process;
    "thread",     string_of_int e.thread;
    "sec",        string_of_int' tm.Unix.tm_sec;
    "min",        string_of_int' tm.Unix.tm_min;
    "hour",       string_of_int' tm.Unix.tm_hour;
    "mday",       string_of_int tm.Unix.tm_mday;
    "month",      string_of_int' (succ tm.Unix.tm_mon);
    "monthname",  string_of_month (succ tm.Unix.tm_mon);
    "monthnm",    string_of_month_abrv (succ tm.Unix.tm_mon);
    "year",       string_of_int (tm.Unix.tm_year + 1900);
    "wday",       string_of_wday tm.Unix.tm_wday;
    "time",       Int64.to_string (time64 e.timestamp);
    "relative",   string_of_int e.relative;
    "level",      Level.to_string e.level;
    "logger",     Name.to_string e.logger;
    "origin",     Name.to_string e.origin;
    "file",       if e.file <> "" then e.file else String.copy "<nofile>";
    "filebase",   if e.file <> "" then Filename.basename e.file else String.copy "<nofile>";
    "line",       string_of_int e.line;
    "column",     string_of_int e.column;
    "message",    e.message;
    "properties", string_of_properties e.properties;
    "exception",  (match e.error with Some (x, _) -> Printexc.to_string x | None -> "");
    "backtrace",  (match e.error with Some (_, x) -> x | None -> "") ]
  @ e.properties

let decode s =
  try
    let idx = String.index s ':' in
    let len = String.length s in
    let key = String.sub s 0 idx in
    let size = String.sub s (succ idx) (len - idx - 1) in
    key, (try int_of_string size with _ -> 0)
  with Not_found -> s, 0

let assoc l s =
  try
    let key, size = decode s in
    let res = List.assoc key l in
    let left = size < 0 in
    let pad_size = (abs size) - (String.length res) in
    let pad_string = String.make (max 0 pad_size) ' ' in
    if left then pad_string ^ res else res ^ pad_string
  with _ ->
    ""

let render_bindings b fmt =
  let buffer = Buffer.create 256 in
  try
    Buffer.add_substitute buffer (assoc b) fmt;
    Buffer.contents buffer
  with _ -> fmt

let render fmt e =
  render_bindings (bindings e) fmt
