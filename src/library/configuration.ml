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


type section = {
    name     : string list;
    elements : (string * string) list;
  }

type t = section list

exception Exception of int * string

let trim s =
  let white_space = function
    | ' ' | '\t' -> true
    | _ -> false in
  let len = String.length s in
  let i = ref 0 in
  let j = ref (pred len) in
  while !i < len && white_space s.[!i] do incr i done;
  while !j >= 0 && white_space s.[!j] do decr j done;
  if (!i <= !j) then
    String.sub s !i (!j - !i + 1)
  else
    ""

let dot = Str.regexp (Str.quote ".")

let normalize s =
  match Str.split dot s with
  | [] -> [""]
  | l -> List.map trim l

let remove_comment s =
  try
    String.sub s 0 (String.index s '#')
  with Not_found ->
    s

let remove_optional_quotes s =
  let len = String.length s in
  if len > 1 then
    let first = s.[0] in
    let last = s.[pred len] in
    match first, last with
    | '\"', '\"'
    | '\'', '\'' -> String.sub s 1 (len - 2)
    | _ -> s
  else
    s

let load filename =
  let first = ref true in
  let sections = ref [] in
  let curr_name = ref "" in
  let curr_elems = ref [] in
  let add_section () =
    if not (!first && (!curr_elems = [])) then
      sections := { name = normalize !curr_name;
                    elements = List.rev !curr_elems } :: !sections in
  let ch = open_in filename in
  let line_no = ref 0 in
  let fail s = raise (Exception (!line_no, s)) in
  try
    while true do
      let line = input_line ch in
      incr line_no;
      let content = trim (remove_comment line) in
      let len = String.length content in
      if len > 0 then begin
        if content.[0] = '[' && content.[pred len] = ']' then begin
          add_section ();
          curr_name := trim (String.sub content 1 (len - 2));
          first := false;
          curr_elems := [];
        end else
          try
            let idx = String.index content '=' in
            let name = String.sub content 0 idx in
            let value = String.sub content (succ idx) (len - idx - 1) in
            if name = "" then fail "empty property name";
            let elem = trim name, remove_optional_quotes (trim value) in
            curr_elems := elem :: !curr_elems
          with Not_found -> fail "invalid line"
      end
    done;
    assert false
  with
  | End_of_file ->
      add_section ();
      close_in_noerr ch;
      List.rev !sections
  | e ->
      close_in_noerr ch;
      raise e
