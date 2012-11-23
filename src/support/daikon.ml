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


(* Definitions *)

let magic_trace = "Daikon trace\000(magic\001)"

let magic_kind = "Daikon kind\000(magic\001)"

let t = magic_trace

let () = Utils.daikon_t := t

type variable = (string * string) list

type properties = (string * string) list

type 'a variable_builder = string -> 'a -> variable


(* Variable constructors *)

let string_of_bool value =
  if value then "true" else "false"

let string_of_string value =
  Printf.sprintf "%S" value

let list_of_option = function
  | Some x -> [ x ]
  | None -> []

external identity : 'a -> 'a = "%identity"

let make id conv =
  let base =
    fun name value ->
      [(Printf.sprintf ">%c%S" (Char.lowercase id) name),
       (conv value)] in
  let container f =
    fun name value ->
      let value = f value in
      [(Printf.sprintf ">%c%S[..]" (Char.uppercase id) name),
       (Printf.sprintf "[%s]" (String.concat " " (List.map conv value)))] in
  base,
  container list_of_option,
  container identity,
  container Array.to_list
    
let bool, bool_option, bool_list, bool_array =
  make 'b' string_of_bool

let int, int_option, int_list, int_array =
  make 'i' string_of_int

let float, float_option, float_list, float_array =
  make 'f' string_of_float

let string, string_option, string_list, string_array =
  make 's' string_of_string


(* Variable combinators *)

let make_variable_builder f =
  fun name value ->
    let vars : variable list = f value in
    let vars = List.flatten vars in
    List.map
      (fun (n, v) ->
        let insert = name ^ "__" in
        let len_n = String.length n in
        let len_insert = String.length insert in
        let len = len_n + len_insert in
        let n' = String.create len in
        String.blit n 0 n' 0 3;
        String.blit insert 0 n' 3 len_insert;
        String.blit n 3 n' (3 + len_insert) (len_n - 3);
        n', v)
      vars

let tuple2 a_vb b_vb =
  make_variable_builder
    (fun (a_v, b_v) ->
      [ a_vb "0" a_v ;
        b_vb "1" b_v ])

let tuple3 a_vb b_vb c_vb =
  make_variable_builder
    (fun (a_v, b_v, c_v) ->
      [ a_vb "0" a_v ;
        b_vb "1" b_v ;
        c_vb "2" c_v ])

let tuple4 a_vb b_vb c_vb d_vb =
  make_variable_builder
    (fun (a_v, b_v, c_v, d_v) ->
      [ a_vb "0" a_v ;
        b_vb "1" b_v ;
        c_vb "2" c_v ;
        d_vb "3" d_v ])

let tuple5 a_vb b_vb c_vb d_vb e_vb =
  make_variable_builder
    (fun (a_v, b_v, c_v, d_v, e_v) ->
      [ a_vb "0" a_v ;
        b_vb "1" b_v ;
        c_vb "2" c_v ;
        d_vb "3" d_v ;
        e_vb "4" e_v ])


(* Properties constructors *)

let point name l =
  (magic_kind, name ^ ":::POINT") :: (List.flatten l)

let enter name l =
  (magic_kind, name ^ ":::ENTER") :: (List.flatten l)

let exit name vars l =
  List.iter
    (fun (var_name, _) ->
      let var_name = String.copy var_name in
      var_name.[0] <- '<')
    vars;
  (magic_kind, name ^ ":::EXIT1") :: vars @ (List.flatten l)


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

module StringSet = Set.Make (String)

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

let layout_decls = decls_header, [], decls_render

let layout_dtrace = dtrace_header, [], dtrace_render

let () =
  List.iter
    (fun (x, y) -> Layout.register x y)
    [ "daikon_decls",  layout_decls ;
      "daikon_dtrace", layout_dtrace ]
