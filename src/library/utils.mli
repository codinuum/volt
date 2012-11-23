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

(** This module regroups miscellaneous utility functions. *)

val verbose : string -> unit
(** Prints the passed string if Bolt verbose mode is enabled,
    does nothing otherwise. *)

type 'a container
(** The type of container mapping a string to an element,
    whatever their implementation is. *)

val make_container_functions : unit -> 'a container * (string -> 'a -> unit) * ('a -> string) * (string -> 'a)
(** [make_container_functions ()] returns a quadruple with the following
    elements:
    - a container;
    - a function that adds a named element to the container;
    - a function that adds an unnamed element to the container,
      and returns its name;
    - a function that returns the element whose name is passed,
      raising [Not_found] if such an element does not exist. *)

val get_thread_id : unit -> int
(** Returns the identifier of the current thread.*)

val enter_critical_section : unit -> unit
(** To be called when entering a critical section. *)

val leave_critical_section : unit -> unit
(** To be called when leaving a critical section. *)

val split : string -> string -> string list
(** [split seps s] returns a list of substrings of [s] separeted by
    contiguous substrings of elements from [seps]. *)

val trim_left : string -> string
(** Returns a copy of the string without leading whitespace. *)

val trim_right : string -> string
(** Returns a copy of the string without trailing whitespace. *)

val trim : string -> string
(** Returns a copy of the string without leading and trailing whitespace. *)


(**/**)

val register_thread_functions : (unit -> int) -> (unit -> unit) -> (unit -> unit) -> unit
(** [register_hooks id enter leaver] registers the passed functions
    allowing Bolt to be made thread safe:
    - [id] to return the idenfier of the current thread;
    - [enter] to be called when entering a critical section;
    - [leave] to be called when leaving a critical section. *)

val paje_t : string ref
(** The message used by the Pajé layout. *)

val daikon_t : string ref
(** The message used by the Daikon layout. *)
