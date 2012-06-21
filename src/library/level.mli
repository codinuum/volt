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

(** This module defines the various level of log, as well as some utility
    functions over levels. *)


type error =
  | Invalid_level_string of string
  | Invalid_level_int of int

exception Exception of error

type t =
  | FATAL (** Level for errors leading to program termination. *)
  | ERROR (** Level for errors handled by the program. *)
  | WARN  (** Level for for hazardous circumstances. *)
  | INFO  (** Level for coarse-grained information. *)
  | DEBUG (** Level for debug information. *)
  | TRACE (** Level for fine-grained information. *)
(** The type of logging levels. *)

val levels : t list
(** The list of all levels, in ascending order. *)

val to_string : t -> string
(** Converts the passed level into a string. *)

val of_string : string -> t
(** Converts the passed string into a level.
    The string is converted from its uppercase form.

    Raises [Exception] if the passed string is not valid. *)

val to_int : t -> int
(** Converts the passed level into an integer. *)

val of_int : int -> t
(** Converts the passed integer into a level.

    Raises [Exception] if the passed integer is not valid. *)
