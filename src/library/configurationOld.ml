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
    | '\'', '\'' -> true, String.sub s 1 (len - 2)
    | _ -> false, s
  else
    false, s

type error =
  | Empty_property_name
  | Invalid_line

let string_of_error = function
  | Empty_property_name -> "empty property name"
  | Invalid_line -> "invalid line"

let load filename =
  let first = ref true in
  let sections = ref [] in
  let curr_name = ref "" in
  let curr_elems = ref [] in
  let add_section () =
    if not (!first && (!curr_elems = [])) then
      sections := { Configuration.name = Name.of_string !curr_name;
                    Configuration.elements = List.rev !curr_elems } :: !sections in
  let ch = open_in filename in
  let line_no = ref 0 in
  let fail s =
    raise (Configuration.Exception (!line_no, string_of_error s)) in
  try
    while true do
      let line = input_line ch in
      incr line_no;
      let contents = Utils.trim (remove_comment line) in
      let len = String.length contents in
      if len > 0 then begin
        if contents.[0] = '[' && contents.[pred len] = ']' then begin
          add_section ();
          curr_name := Utils.trim (String.sub contents 1 (len - 2));
          first := false;
          curr_elems := [];
        end else
          try
            let idx = String.index contents '=' in
            let name = String.sub contents 0 idx in
            let value = String.sub contents (succ idx) (len - idx - 1) in
            if name = "" then fail Empty_property_name;
            let name = Utils.trim name in
            let quoted, value = remove_optional_quotes (Utils.trim value) in
            let value =
              if quoted then
                Configuration.String value
              else
                try
                  Configuration.Integer (int_of_string value)
                with _ ->
                  try
                    Configuration.Float (float_of_string value)
                  with _ -> 
                    Configuration.Identifier value in
            let elem = name, value in
            curr_elems := elem :: !curr_elems
          with Not_found -> fail Invalid_line
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
