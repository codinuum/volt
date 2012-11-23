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

(** This module defines the modes of event delivery. *)


type error =
  | Invalid_condition_string of string

exception Exception of error

class type t =
  object
    method deliver : Output.impl -> string -> unit
    method flush : Output.impl -> unit
  end
(** The type of modes of events delivery. *)

val direct : unit -> t
(** Returns a mode delivering events as soon as they are generated. *)

val memory : unit -> t
(** Returns a mode delivering all events at program termination. *)

val retained : string -> t
(** Returns a mode delivering events every time a condition, specified by
    the string parameter is made true. The condition can have one of the
    following form:
    - an integer followed by the letter 'e' (e. g. "10e") means that
      events are delivered when {i n} have been accumulated;
    - an integer followed by the letter 'b' (e. g. "10b") means that
      events are delivered when a string representation of {i n} bytes
      has been accumulated;
    - an integer followed by the letter 's' (e. g. "10s") means that
      events are delivered every {i n} seconds. *)
