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

(** This module defines the concept of output, that is how an event is
    written to a channel. It is possible to register new outputs through
    the [register] function. Initially, two outputs are registered:
    - ["file"] for file output;
    - ["void"] for an output actually discarding data. *)


(** {6 Definitions} *)

class type impl =
  object
    method write : string -> unit
    (** Method called to actually write data.
        This method is reponsible for rotation handling. *)
    method close : unit
    (** Method called to close the underlying data container. *)
  end
(** The type of output objects, that is concrete implementation. *)

type rotation = {
    seconds_elapsed : float option; (** Number of seconds between two rotations. *)
    signal_caught : Signal.t option; (** Number of signal provoking a rotation. *)
  }
(** The type of rotation conditions, rotation occurring when one of
    the conditions is met. *)

type t = string -> rotation -> Layout.t lazy_t -> impl
(** The type of outputs, that is a function constructing an actual
    output implementation. The first parameter describes the output
    ({i e. g.} filename), while the second one is an optional {i rotation}
    value ({i e. g.} time between two file switches). The exact semantics
    associated with both parameters is output-dependent.
    The third argument is the layout associated with the logger, it is
    needed to get header and footer (that can be necessary because some
    outputs may want to write them at each rotation.) *)

val register : string -> t -> unit
(** [register n o] registers the output [o] with name [n],
    replacing any existing output with the same name. *)

val register_unnamed : t -> string
(** Similar to [register] except that an unused name is generated and returned. *)

val get : string -> t
(** [get n] returns the output registered with name [n].

    Raises [Not_found] if no output exists with the passed name. *)


(** {6 Predefined outputs} *)

val void : t
(** The output initially registered with the name "void".
    Discards all data; all three parameters being ignored. *)

val file : t
(** The output initially registered with the name "file".
    Uses bare files, writing header and footer at each rotation (if any).
    The rotation is given by the float parameter and is measured in
    seconds. The string parameter is essentially interpreted as a file
    name, except that:
    - ["<stdout>"] is interpreted as the standard output (rotation being
      disabled);
    - ["<stderr>"] is interpreted as the standard error (rotation being
      disabled);
    - the "%" character is substituted with a string acting as a timestamp
      (precisely: "YEAR-MONTH-DAY-HOUR-MINUTES-SECONDS-MILLISECONDS"),
      thus useful when rotating files (otherwise, the same file would be
      written over and over again);
    - ["$(time)"] is equivalent to "%";
    - ["$(pid)"] is substituted with process identifier;
    - ["$(hostname)"] is substituted with hostname identifier;
    - ["$(var)"] is substituted with environment variable named ["var"].

    I/O errors are silently discarded (unless ["BOLT_SILENT"] is not set
    to either ["YES"] or ["ON"] - ignoring case). *)

val growlnotify : t
(** The output initially registered with the name "growlnotify".
    Sends the data to Growl through the [growlnotify] command-line
    utility. *)

val bell : t
(** The output initially registered with the name "bell".
    Discards all data, just writes the "bell" character on standard
    output. *)

val say : t
(** The output initially registered with the name "say".
    Actually says the data through the [say] command-line utility
    (MacOS X). *)

(**/**)

val signals : bool array
(** Whether signals were caught.

    {b FOR INTERNAL USE} *)
