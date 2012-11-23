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

(** This module defines the contents of a configuration file. *)


type value =
  | Identifier of string (** An identifier value. *)
  | Integer of int (** An integer value. *)
  | Float of float (** A float value. *)
  | String of string (** A string value, unescaped. *)
  | And of value * value (** v1 && v2 *)
  | Or of value * value (** v1 || v2 *)
(** The type of property values. *)

type section = {
    name : Name.t; (** Section name. *)
    elements : (string * value) list; (** Key, value assocation list of section properties. *)
  }
(** The type of a configuration section. *)

type t = section list
(** The type of configuration file contents. *)

exception Exception of int * string
(** Exception to be raised if file loading fails.
    The first parameter is the line of the error, while the second
    parameter is a short description of the error. *)
