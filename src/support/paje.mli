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

(** This module provides support for the Pajé tool.

    Pajé (available at {i https://gforge.inria.fr/projects/paje/}) is
    a graphical tool for the analysis of multithreads and/or multiprocesses
    programs.

    Bolt can produce Pajé-comptabile traces by using statements like:
    {C [LOG Paje.t WITH Paje.new_event ...;] }
    more information can be found in the Bolt manual.
 *)


(** {6 Definitions} *)

val t : string
(** The identifier message for Paje event. *)

type properties = (string * string) list
(** The type of information to be recorded in a trace. *)


(** {6 Predefined events} *)

val define_container_type : name:string -> ?alias:string -> container_type:string -> properties -> properties

val create_container : name:string -> ?alias:string -> typ:string -> container:string -> properties -> properties

val destroy_container : name:string -> typ:string -> properties -> properties

val define_event_type : name:string -> ?alias:string -> container_type:string -> properties -> properties

val define_state_type : name:string -> ?alias:string -> container_type:string -> properties -> properties

val define_variable_type : name:string -> ?alias:string -> container_type:string -> properties -> properties

val define_link_type : name:string -> ?alias:string -> container_type:string -> source_container_type:string -> dest_container_type:string -> properties -> properties

val define_entity_value : name:string -> ?alias:string -> entity_type:string -> properties -> properties

val new_event : typ:string -> container:string -> value:string -> properties -> properties

val set_state : typ:string -> container:string -> value:string -> properties -> properties

val push_state : typ:string -> container:string -> value:string -> properties -> properties

val pop_state : typ:string -> container:string -> properties -> properties

val set_variable : typ:string -> container:string -> value:float -> properties -> properties

val add_variable : typ:string -> container:string -> value:float -> properties -> properties

val sub_variable : typ:string -> container:string -> value:float -> properties -> properties

val start_link : typ:string -> container:string -> source_container:string -> value:string -> key:string -> properties -> properties

val end_link : typ:string -> container:string -> dest_container:string -> value:string -> key:string -> properties -> properties


(** {6 Layout elements} *)

val header : string list
(** The header defining Paje events. *)

val render : Event.t -> string
(** The rendering function for Paje format. *)
