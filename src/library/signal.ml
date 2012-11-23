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


type t =
  | HUP
  | USR1
  | USR2

type error =
  | Invalid_signal_string of string
  | Invalid_signal_int of int
  | Invalid_signal_sys of int

let string_of_error = function
  | Invalid_signal_string s -> Printf.sprintf "invalid signal string %S" s
  | Invalid_signal_int i -> Printf.sprintf "invalid signal int \"%d\"" i
  | Invalid_signal_sys i -> Printf.sprintf "invalid signal Sys code \"%d\"" i

exception Exception of error

let () =
  Printexc.register_printer
    (function
      | Exception error -> Some (string_of_error error)
      | _ -> None)

let fail error =
  raise (Exception error)

let of_string s =
  match String.lowercase s with
  | "sighup" -> HUP
  | "sigusr1" -> USR1
  | "sigusr2" -> USR2
  | _ -> fail (Invalid_signal_string s)

let to_sys = function
  | HUP -> Sys.sighup
  | USR1 -> Sys.sigusr1
  | USR2 -> Sys.sigusr2

let of_sys x =
  if x = Sys.sighup then HUP
  else if x = Sys.sigusr1 then USR1
  else if x = Sys.sigusr2 then USR2
  else fail (Invalid_signal_sys x)

let to_int = function
  | HUP -> 0
  | USR1 -> 1
  | USR2 -> 2

let of_int = function
  | 0 -> HUP
  | 1 -> USR1
  | 2 -> USR2
  | s -> fail (Invalid_signal_int s)

let max_int = 2
