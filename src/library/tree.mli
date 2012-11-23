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

(** This module is responsible for the management of loggers, stored in a
    tree. *)


type logger_info = {
    name : Name.t; (** Logger name. *)
    level : Level.t; (** Logger level, filtering events with a greater level. *)
    filter : Filter.t lazy_t; (** Logger filter, selecting events to record. *)
    pass : Filter.t lazy_t; (** Logger pass filter, selecting events to record. *)
    layout : Layout.t lazy_t; (** Logger layout, defining how events are recorded. *)
    mode : Mode.t; (** Logger mode, defining when events are recorded. *)
    output : Output.impl lazy_t; (** Logger output, defining where events are recorded. *)
  }
(** The type of loggers. *)

val register_logger : logger_info -> unit
(** Registers the passed logger, making it able to receive events. *)

val get_loggers : Name.t -> (Name.t * logger_info list) list
(** Returns the list of all loggers that may receive an event initially
    sent to the loggers whose name is passed. Each element of the
    returned list is a [(n, l)] couple where all loggers appearing in
    [l] are guaranteed to have a name equal to [n]. *)

val make_node : Name.t -> unit
(** Creates a node for holding loggers whose name is passed.
    Node are automatically created by calls to either [register_logger],
    or [get_loggers], hence calling [make_node] only performs the node
    creation earlier. *)
