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


type value =
  | Identifier of string
  | Integer of int
  | Float of float
  | String of string
  | And of value * value
  | Or of value * value

type section = {
    name : Name.t;
    elements : (string * value) list;
  }

type t = section list

exception Exception of int * string

let () =
  Printexc.register_printer
    (function
      | Exception (l, m) ->
          let msg = Printf.sprintf "configuration error at line %d: %s" l m in
          Some msg
      | _ -> None)
