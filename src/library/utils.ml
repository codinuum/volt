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

let register_thread_functions f g h =
  thread_id := f;
  hook_before := g;
  hook_after := h
