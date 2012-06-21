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

(** This module defines the contents of a configuration file,
    as well as a function parsing such files. *)


type section = {
    name : string list; (** Section name, the elements of the list being the component of the name (dot-separated). *)
    elements : (string * string) list; (** Key, value assocation list of section properties. *)
  }
(** The type of a configuration section. *)

type t = section list
(** The type of configuration file contents. *)

exception Exception of int * string
(** Exception to be raised if file loading fails.
    The first parameter is the line of the error, while the second parameter
    is a short description of the error. *)

val normalize : string -> string list
(** Normalizes a section name by splitting it at each dot. *)

val load : string -> t
(** Loads the configuration defined by the the passed file.

    The file format is as follows:
    - the format is line-oriented;
    - comments start with the '#' character and end at the end of the line;
    - sections start with a line of the form "[a.b.c]", "a.b.c" being the name of the section;
    - a section ends when a new section starts;
    - at the beginning of the file, the section named "" is currently opened;
    - section properties are defined by lines of the form "key=value";
    - others lines should be empty (only populated with whitespaces and comments).

    Raises [Sys_error] if an i/o error occurs, and [Exception] if file cannot be parsed. *)
