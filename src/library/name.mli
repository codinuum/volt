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

(** This module defines the type of logger names, as well as conversion
    function from/to string. A logger name is a dot-separated list of
    strings. *)


type t
(** The type of logger names. *)

val of_string : string -> t
(** Converts a string into a logger name. *)

val of_list : string list -> t
(** Converts a string list into a logger name. *)

val to_string : t -> string
(** Converts a logger name into a string. *)

val to_list : t -> string list
(** Converts a logger name into a string list. *)
