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

(** This module defines the concept of logger.

    Loggers are arranged in a hierarchy, according to their names.
    Logger names are dot-separated identifiers akin to OCaml module
    names, the hierarchy between loggers is defined the same way as it
    is defined between modules. It is thus considered good practice to
    define a logger per module and to have the logger name equal to the
    module name. The parent of all loggers with no dot is the logger
    named [""] (empty string).

    Another good practice is to never call directly the functions of this
    module. Indeed, registration of loggers should be done by setting
    either the environment variable ["BOLT_CONFIG"] (for the new
    configuration format), or the environment variable ["BOLT_FILE"] (for
    the old configuration format). The formats of such files are defined
    respectively in modules [ConfigurationNew], and [ConfigurationOld].
    More information can be found in the Bolt user manual.

    Also, actual logging should be done using the camlp4 syntax extension
    named bolt_pp; it allows to easily turn logging off to avoid any
    runtime penalty for production use.

    Bolt writes error messages on the standard error, except if the
    environment variable ["BOLT_SILENT"] is set to either ["YES"] or
    ["ON"] (case insensitive), defaulting to ["NO"]. *)


val register : string -> Level.t -> string -> string -> string -> Mode.t -> string ->
  (string * Output.rotation) -> unit
(** [register nm lv fl ps ly m out (n, r)] registers a logger with name [nm],
    level [lv], filter named [fl], pass filter named [ps], mode [m], layout named [ly], and
    output named [out]. [n] and [r] are the parameter passed for output
    instance creation. The logger will write any event with a level below
    or equal to [lv] that also make the filter [fl] return [true]. Such
    events are written to output [out] through mode [m], using layout
    [ly]. *)

val log : string -> Level.t -> ?file:string -> ?line:int -> ?column:int ->
  ?properties:(string * string) list -> ?error:exn option -> string -> unit
(** [log nm lv ~file:f ~line:l ~column:c ~properties:p ~error:e msg]
    produces an event of level [lv], file [f], line [l], column [c],
    properties [p], error [e], and message [msg]. The event is presented
    to the logger named [nm] if it exists as well as to all its existing
    parents. Each logger will decide if it records the event according
    to its level and filter. *)

val logf : string -> Level.t -> ?file:string -> ?line:int -> ?column:int ->
  ?properties:(string * string) list -> ?error:exn option -> 
    ('a, unit, string, unit) format4 -> 'a
(** [log nm lv ~file:f ~line:l ~column:c ~properties:p ~error:e fmt]
    produces an event of level [lv], file [f], line [l], column [c],
    properties [p], error [e], and message format [fmt]. The event is presented
    to the logger named [nm] if it exists as well as to all its existing
    parents. Each logger will decide if it records the event according
    to its level and filter. *)

val check_level : string -> Level.t -> bool
(** [check_level nm lv] checks if an event of level [lv] is recorded by 
    the logger named [nm] or its ancestors. *)

val prepare : string -> unit
(** [prepare nm] indicates that a module is about to use a logger with
    name [nm]. It is not mandatory to call this function before using
    a logger, but doing so allows the runtime to setup the appropriate
    elements for faster accesses. *)
