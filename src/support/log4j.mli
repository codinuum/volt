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

(** This module provides support for the log4j tool.

    Log4j (available at {i http://logging.apache.org/log4j/}) is the
    {i de facto} standard for logging on the Java platform. *)


val header : string list
(** The header defining log4j elements (actually empty). *)

val render : Event.t -> string
(** The rendering function for log4j format. *)

val layout : Layout.t
(** The xml layout, compatible with the one defined by Apache log4j
    (cf. http://logging.apache.org/log4j/). *)
