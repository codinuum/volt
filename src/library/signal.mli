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

(** This module provides support for signal handling. *)

type t =
  | HUP (** Equivalent to [Sys.sighup]. *)
  | USR1 (** Equivalent to [Sys.sigusr1]. *)
  | USR2 (** Equivalent to [Sys.sigusr2]. *)
(** The type of signal that can trigger log rotate. *)

type error =
  | Invalid_signal_string of string
  | Invalid_signal_int of int
  | Invalid_signal_sys of int

exception Exception of error

val of_string : string -> t
(** Converts a string into a signal.

    Raises [Exception] if the passed string is invalid. *)

val to_sys : t -> int
(** Converts a signal into its [Sys] equivalent. *)

val of_sys : int -> t
(** Converts a [Sys] integer code into a signal.

    Raises [Exception] if the passed integer is invalid. *)

val to_int : t -> int
(** Converts a signal into an integer between [0] and [max_int - 1]. *)

val of_int : int -> t
(** Converts an integer into a signal.

    Raises [Exception] if the passed integer is invalid (below 0, or
    above or equal to [max_int]). *)

val max_int : int
(** Maximal integer value for a signal. *)
