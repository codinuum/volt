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

(** This module defines the concept of event and how to construct them. *)


type time
(** The type of event timestamps. *)

type t = private {
    id : int; (** Event identifier. *)
    hostname : string; (** Host name of running program. *)
    process : int; (** Process identifier of running program. *)
    thread : int; (** Thread identifier of event source. *)
    timestamp : time; (** Event timestamp. *)
    relative : int; (** Time elapsed between application start and event construction. *)
    level : Level.t; (** Event level. *)
    logger : Name.t; (** Logger name. *)
    origin : Name.t; (** First logger that received the event. *)
    file : string; (** Location of event source. *)
    line : int; (** Location of event source. *)
    column : int; (** Location of event source. *)
    message : string; (** Event message. *)
    properties : (string * string) list; (** Event properties as an association list from keys to values. *)
    error : (exn * string) option; (** Event error (parameters being actual exception and exception backtrace). *)
  }
(** The type of log events. *)

val make : Name.t -> Level.t -> ?origin:Name.t option -> ?file:string -> ?line:int -> ?column:int -> ?properties:(string * string) list -> ?error:exn option -> string -> t
(** [make lg lv ~origin:o ~file:fn ~line:ln ~column:cl ~properties:p ~error:e m]
    constructs a log event for logger [lg] with level [lv], location being
    defined by [fn] (filename), [ln] (line) and [cl] (column). [p] is an
    association list providing user-defined properties, [e] is an
    optional exception to be recorded (with its backtrace), and [o] is
    the first logger receiving the event (when [None], the origin is set
    to [lg]). Identifier, thread, timestamp, relative time, and backtrace
    are automatically generated. *)

val with_logger : Name.t -> t -> t
(** [with_logger l e] returns an event that is identical to [e], except
    that its logger is equal to [l]. *)

val bindings : t -> (string * string) list
(** Returns the bindings for the passed event.
    The bindings are an association list to be used for event rendering.
    The following keys are defined:
    - ["id"] event identifier;
    - ["hostname"] host name of running program;
    - ["process"] process identifier of running program ({i a.k.a.} pid);
    - ["thread"] thread identifier;
    - ["sec"] seconds of event timestamp;
    - ["min"] minutes of event timestamp;
    - ["hour"] hour of event timestamp;
    - ["mday"] day of month of event timestamp;
    - ["month"] month of year of event timestamp (as a number);
    - ["monthname"] month name of year of event timestamp (as a string);
    - ["monthnm"] abbreviated month name of year of event timestamp (as a string);
    - ["year"] year of event timestamp (on four digits);
    - ["wday"] day of week of event timestamp (as a string);
    - ["time"] event timestamp (epoch-based);
    - ["relative"] time elapsed between initilization and event creation;
    - ["level"] event level;
    - ["logger"] event logger;
    - ["origin"] first logger that received the event;
    - ["file"] event file (using ["<nofile>"] instead of the empty
      string);
    - ["filebase"] event file (without directory information);
    - ["line"] event line;
    - ["column"] event column;
    - ["message"] event message;
    - ["properties"] property list of event
      (format: ["[k1: v1; ...; kn: vn]"]);
    - ["exception"] event exception;
    - ["backtrace"] event exception backtrace.

    These keys have precedence over the one given at event creation. *)

val render_bindings : (string * string) list -> string -> string
(** [render_bindings l fmt] returns a string representing [fmt] where all
    bindings of the association list [l] have been expanded. Bindings,
    and their textual format are defined in [render]. *)

val render : string -> t -> string
(** [render fmt e] returns a string representing [fmt] where all bindings
    of [e] have been expanded. Bindings should appear in [fmt] is the
    [$(id:pad)] format where [id] is the binding key, and pad the optional
    padding. The padding consists of whitespace, on the right if [pad] is
    positive and on the left otherwise. *)
