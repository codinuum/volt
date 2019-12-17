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


type error =
  | Invalid_level_string of string
  | Invalid_level_int of int

let string_of_error = function
  | Invalid_level_string s -> Printf.sprintf "invalid level string %S" s
  | Invalid_level_int i -> Printf.sprintf "invalid level int \"%d\"" i

exception Exception of error

let () =
  Printexc.register_printer
    (function
      | Exception error -> Some (string_of_error error)
      | _ -> None)

let fail error =
  raise (Exception error)

type t =
  | FATAL
  | ERROR
  | WARN
  | INFO
  | DEBUG
  | TRACE

let levels = [
  FATAL ;
  ERROR ;
  WARN ;
  INFO ;
  DEBUG ;
  TRACE
]

let to_string = function
  | FATAL -> "FATAL"
  | ERROR -> "ERROR"
  | WARN -> "WARN"
  | INFO -> "INFO"
  | DEBUG -> "DEBUG"
  | TRACE -> "TRACE"

let of_string x =
  match String.uppercase_ascii x with
  | "FATAL" -> FATAL
  | "ERROR" -> ERROR
  | "WARN" -> WARN
  | "INFO" -> INFO
  | "DEBUG" -> DEBUG
  | "TRACE" -> TRACE
  | _ -> fail (Invalid_level_string x)

let to_int = function
  | FATAL -> 0
  | ERROR -> 1
  | WARN -> 2
  | INFO -> 3
  | DEBUG -> 4
  | TRACE -> 5

let of_int = function
  | 0 -> FATAL
  | 1 -> ERROR
  | 2 -> WARN
  | 3 -> INFO
  | 4 -> DEBUG
  | 5 -> TRACE
  | x -> fail (Invalid_level_int x)
