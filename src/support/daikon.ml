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

let magic_trace = "Daikon trace\000(magic\001)"

let magic_kind = "Daikon kind\000(magic\001)"

let t = magic_trace

type variable = string * string

type properties = (string * string) list


(* Variable constructors *)

let string_of_bool value =
  if value then "true" else "false"

let string_of_string value =
  Printf.sprintf "%S" value

external identity : 'a -> 'a = "%identity"

let make id conv =
  let base =
    fun name value ->
      (Printf.sprintf ">%c%S" (Char.lowercase id) name),
      (conv value) in
  let container f =
    fun name value ->
      let value = f value in
      (Printf.sprintf ">%c%S[..]" (Char.uppercase id) name),
      (Printf.sprintf "[%s]" (String.concat " " (List.map conv value))) in
  base,
  container identity,
  container Array.to_list
    
let bool, bool_list, bool_array = make 'b' string_of_bool

let int, int_list, int_array = make 'i' string_of_int

let float, float_list, float_array = make 'f' string_of_float

let string, string_list, string_array = make 's' string_of_string
     

(* Properties constructors *)

let point name l =
  (magic_kind, name ^ ":::POINT") :: l

let enter name l =
  (magic_kind, name ^ ":::ENTER") :: l

let exit name (var_name, var_value) l =
  let var_name = String.copy var_name in
  var_name.[0] <- '<';
  (magic_kind, name ^ ":::EXIT1") :: (var_name, var_value) :: l


(* Layout elements *)

let decls_header = [
  "// generated through Bolt " ^ Version.value;
  "";
  "decl-version 2.0";
  "input-language OCaml";
  "var-comparability implicit";
  ""
]

let extract_kind l = List.partition (fun (x, _) -> x = magic_kind) l

module StringSet = Set.Make (struct type t = string let compare = Pervasives.compare end)

let already_seen = ref StringSet.empty

let decls_render e =
  if e.Event.message = magic_trace then
    let kind, rest = extract_kind e.Event.properties in
    match kind with
    | [_, name] ->
        Utils.enter_critical_section ();
        let seen = StringSet.mem name !already_seen in
        already_seen := StringSet.add name !already_seen;
        Utils.leave_critical_section ();
        if not seen then begin
          let buff = Buffer.create 512 in
          let add_string s =
            Buffer.add_string buff s;
            Buffer.add_char buff '\n' in
          let add_strings s s' =
            Buffer.add_string buff s;
            Buffer.add_char buff ' ';
            Buffer.add_string buff s';
            Buffer.add_char buff '\n' in
          add_strings "ppt" name;
          let ppt_type =
            try
              let idx = String.rindex name ':' in
              let len = String.length name in
              match String.sub name (succ idx) (len - idx - 1) with
              | "POINT" -> "point"
              | "ENTER" -> "enter"
              | "EXIT1" -> "exit"
              | _ -> failwith "unsupport point type"
            with _ -> "point" in
          add_strings "ppt-type" ppt_type;
          List.iter
            (fun (k, _) ->
              try
                let kind = if k.[0] = '<' then "return" else "variable" in
                let dim = if k.[1] = (Char.lowercase k.[1]) then 0 else 1 in
                let dec, rep = match Char.lowercase k.[1] with
                | 'i' -> "int", "int"
                | 'b' -> "bool", "boolean"
                | 'f' -> "float", "double"
                | 's' -> "string", "java.lang.String"
                | _ -> failwith "unsupported type" in
                let k = String.sub k 2 ((String.length k) - 2) in
                add_strings "  variable" k;
                add_strings "    var-kind " (if dim = 1 then "array" else kind);
                add_strings "    array " (string_of_int dim);
                add_strings "    dec-type " (dec ^ (if dim = 1 then "\\_array" else ""));
                add_strings "    rep-type " (rep ^ (if dim = 1 then "[]" else ""));
                add_strings "    comparability " "22"
              with _ -> ())
          (List.sort compare rest);
          add_string "";
          Buffer.contents buff
        end else
          ""
    | _ -> ""
  else
    ""

let dtrace_header = [
  "// generated through Bolt " ^ Version.value;
  "";
]

let dtrace_render e =
  if e.Event.message = magic_trace then
    let kind, rest = extract_kind e.Event.properties in
    match kind with
    | [_, name] ->
        let buff = Buffer.create 512 in
        let add_string s =
          Buffer.add_string buff s;
          Buffer.add_char buff '\n' in
        add_string name;
        List.iter
          (fun (k, v) ->
            if k <> magic_kind then begin
              let k = String.sub k 2 ((String.length k) - 2) in
              add_string k;
              add_string v;
              add_string "1"
            end)
          (List.sort compare rest);
        Buffer.contents buff
    | _ -> ""
  else
    ""
