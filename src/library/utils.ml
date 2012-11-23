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

let verbose s =
  let print s =
    prerr_string " *** Bolt: ";
    prerr_endline s in
  try
    match String.uppercase (Sys.getenv "BOLT_SILENT") with
    | "YES" | "ON" -> ()
    | _ -> print s
  with Not_found -> print s

type 'a container = (string, 'a) Hashtbl.t

let make_container_functions () =
  let id = ref 0 in
  let container =
    Hashtbl.create 17 in
  let register name elem =
    Hashtbl.replace container name elem in
  let register_unnamed elem =
    while Hashtbl.mem container (string_of_int !id) do
      incr id
    done;
    let name = string_of_int !id in
    register name elem;
    name in
  let get name =
    Hashtbl.find container name in
  container, register, register_unnamed, get

let thread_id = ref (fun () -> 0)

let hook_before = ref (fun () -> ())

let hook_after = ref (fun () -> ())

let get_thread_id () = !thread_id ()

let enter_critical_section () =
  !hook_before ()

let leave_critical_section () =
  !hook_after ()

let split seps s =
  let idx = ref 0 in
  let len = String.length s in
  let buff = Buffer.create len in
  let res = ref [] in
  let in_sep = ref false in
  while !idx < len do
    if !in_sep then begin
      if not (String.contains seps s.[!idx]) then begin
        Buffer.add_char buff s.[!idx];
        in_sep := false
      end
    end else begin
      if String.contains seps s.[!idx] then begin
        res := (Buffer.contents buff) :: !res;
        Buffer.clear buff;
        in_sep := true
      end else
        Buffer.add_char buff s.[!idx]
    end;
    incr idx
  done;
  let last = Buffer.contents buff in
  if last <> "" then res := last :: !res;
  List.rev !res

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let trim_gen left right s =
  let i = ref 0 in
  let len = String.length s in
  if left then
    while (!i < len) && (is_whitespace s.[!i]) do
      incr i
    done;
  let j = ref (pred len) in
  if right then
    while (!j >= !i) && (is_whitespace s.[!j]) do
      decr j
    done;
  if j >= i then
    String.sub s !i (!j - !i + 1)
  else
    ""

let trim_left = trim_gen true false

let trim_right = trim_gen false true

let trim = trim_gen true true


let register_thread_functions f g h =
  thread_id := f;
  hook_before := g;
  hook_after := h

let paje_t = ref ""

let daikon_t = ref ""
