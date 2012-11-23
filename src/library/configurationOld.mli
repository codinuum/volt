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

(** This module provides the function loading a configuration from a
    file in "old" format.

    The file format is as follows:
    - the format is line-oriented;
    - comments start with the '#' character and end at the end of the
      line;
    - sections start with a line of the form "[a.b.c]", "a.b.c" being the
      name of the section;
    - a section ends when a new section starts;
    - at the beginning of the file, the section named "" is currently
      opened;
    - section properties are defined by lines of the form "key=value";
    - others lines should be empty (only populated with whitespaces and
      comments). *)


val load : string -> Configuration.t
(** Loads the configuration from the passed file.

    Raises [Sys_error] if an i/o error occurs, and
    [Configuration.Exception] if file cannot be parsed. *)
