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

(** This module provides support for the Pajé tool (version 1.2.3).

    Pajé (available at {i http://paje.sourceforge.net}) is
    a graphical tool for the analysis of multithreads and/or multiprocesses
    programs.

    Bolt can produce Pajé-compatible traces by using statements like:
    {C [LOG Paje.t WITH Paje.new_event ...;] }
    more information can be found in the Bolt manual. *)


(** {6 Definitions} *)

val t : string
(** The identifier message for Pajé event. *)

type properties = (string * string) list
(** The type of information to be recorded in a trace. *)

type name = string
(** Synonym for names. *)

type alias = string
(** Synonym for aliases. *)

type color = float * float * float
(** The type of colors, as three red/green/blue components
    (each should be between [0.0] and [1.0]). *)


(** {6 Predefined events: type definitions} *)

val define_container_type : name:name -> ?typ:string -> ?alias:alias -> properties -> properties
(** Defines a new type of container with name, optional parent type,
    optional alias, and a list of additional properties. *)

val define_state_type : name:name -> typ:string -> ?alias:alias -> properties -> properties
(** Defines a new type of state with name, type, optional alias, and a
    list of additonnal properties. *)

val define_event_type : name:name -> typ:string -> ?alias:alias -> properties -> properties
(** Defines a new type of event with name, type, optional alias, and a
    list of additional properties. *)

val define_variable_type : name:name -> typ:string -> color:color -> ?alias:alias -> properties -> properties
(** Defines a new type of variable with name, type, optional alias, and a
    list of additonnal properties. *)

val define_link_type : name:name -> typ:string -> start_container_type:string -> end_container_type:string -> ?alias:alias -> properties -> properties
(** Defines a new type of link with name, type, start and end container
    types, optional alias, and a list of additonnal properties. *)

val define_entity_value : name:name -> typ:string -> color:color -> ?alias:alias -> properties -> properties
(** Defines a new value for an entity value with name, type, color,
    optional alias, and a list of additional properties. *)


(** {6 Predefined events: trace recording} *)

val create_container : name:name -> typ:string -> ?container:name -> ?alias:alias -> properties -> properties
(** Creates a container with name, type, optional parent container,
    optional alias and a list of additional properties. *)

val destroy_container : name:name -> typ:string -> properties -> properties
(** Destroys a container with name, type, and a list of additional
    properties. *)

val set_state : typ:string -> container:name -> value:string -> properties -> properties
(** Changes the state of a given container to a new value with passed
    type, and additional list of properties. *)

val push_state : typ:string -> container:name -> value:string -> properties -> properties
(** Pushes the current state of a given container to its own stack,
    and changes its state to the passed type and value. *)

val pop_state : typ:string -> container:name -> properties -> properties
(** Changes the states of a given container by poping a previously pushed
    value from its own stack. *)

val reset_state : typ:string -> container:name -> properties -> properties
(** Clears all previously saved values for the type of a container. *)

val new_event : typ:string -> container:name -> value:string -> properties -> properties
(** Records a new event with type, container, value, and additional list
    of properties. *)

val set_variable : typ:string -> container:name -> value:float -> properties -> properties
(** Sets the value of a container variable. *)

val add_variable : typ:string -> container:name -> value:float -> properties -> properties
(** Increases the value of a container variable by a given amount. *)

val sub_variable : typ:string -> container:name -> value:float -> properties -> properties
(** Decreases the value of a container variable by a given amount. *)

val start_link : typ:string -> container:name -> start_container:name -> value:string -> key:string -> properties -> properties
(** Records the start of a link occuring inside a container, from a start.
    The key/value pair is used to match with an associated end of link. *)

val end_link : typ:string -> container:name -> end_container:name -> value:string -> key:string -> properties -> properties
(** Records the end of a link occuring inside a container, to an end.
    The key/value pair is used to match with an associated start of
    link. *)


(** {6 Layout elements} *)

val header : string list
(** The header defining Pajé events. *)

val render : Event.t -> string
(** The rendering function for Pajé format. *)

val layout : Layout.t
(** The layout supporting the "Pajé" trace format.
    The message of the event should match the name of an {i EventDef}
    defined by the "Pajé trace file format" available at 
    http://paje.sourceforge.net/download/publication/lang-paje.pdf.
    The values of the fields are taken from the event properties, the
    ["Time"] field being automatically set to the time elapsed since
    program start (unless explicitly set through a property). *)

val layout_noheader : Layout.t
(* Same as [paje], but without any header.
   Useful to avoid duplicate information when several processes produce
   Pajé traces. *)


(** {6 Functorial interface} *)

type type_kind = Container | State | Event | Variable | Link | Entity_value

exception Invalid_type of type_kind

module type Definitions = sig
  val logger : string
  (** The name of the logger used to register definitions. *)

  val level : Level.t
  (** The level used to register definitions. *)

  type container_type
  (** The type of containers, typically a sum type. *)

  val container_types : (container_type * name * (container_type option) * alias) list
  (** The definition of container types: value, name, optional parent, and alias. *)

  type state_type
  (** The type of states, typically a sum type. *)

  val state_types : (state_type * name * container_type * alias) list
  (** The definition of event types: value, name, container, and alias. *)

  type event_type
  (** The type of events, typically a sum type. *)

  val event_types : (event_type * name * container_type * alias) list
  (** The definition of event types: value, name, container, and alias. *)

  type variable_type
  (** The type of states, typically a sum type. *)

  val variable_types : (variable_type * name * container_type * color * alias) list
  (** The definition of variable types: value, name, container, color, and alias. *)

  type link_type
  (** The type of links, typically a sum type. *)

  val link_types : (link_type * name * container_type * container_type * container_type * alias) list
  (** The definition of link types: value, name, parent container, start container, end container, and alias. *)

  type entity_value_type
  (** The type of entity values, typically a sum type. *)

  val entity_value_types : (entity_value_type * name * container_type * color * alias) list
  (** The definition of entity value types: value, name, parent container, color, and alias. *)
end
(** The input signature of {!Paje.Make}, that is static definition of
    Pajé types. *)

module type S = sig
  val t : string
  type properties = (string * string) list
  type name = string
  type alias = string
  type color = float * float * float
  type container_type
  type state_type
  type event_type
  type variable_type
  type link_type
  type entity_value_type
  val create_container : name:name -> typ:container_type -> ?container:name -> ?alias:alias -> properties -> properties
  val destroy_container : name:name -> typ:container_type -> properties -> properties
  val set_state : typ:state_type -> container:name -> value:string -> properties -> properties
  val push_state : typ:state_type -> container:name -> value:string -> properties -> properties
  val pop_state : typ:state_type -> container:name -> properties -> properties
  val reset_state : typ:state_type -> container:name -> properties -> properties
  val new_event : typ:event_type -> container:name -> value:string -> properties -> properties
  val set_variable : typ:variable_type -> container:name -> value:float -> properties -> properties
  val add_variable : typ:variable_type -> container:name -> value:float -> properties -> properties
  val sub_variable : typ:variable_type -> container:name -> value:float -> properties -> properties
  val start_link : typ:link_type -> container:name -> start_container:name -> value:string -> key:string -> properties -> properties
  val end_link : typ:link_type -> container:name -> end_container:name -> value:string -> key:string -> properties -> properties
end
(** The output signature of {!Paje.Make}, that is type-safe interface
    to Pajé functions. *)

module Make (D : Definitions) : S
  with type container_type = D.container_type
  and type event_type = D.event_type
  and type state_type = D.state_type
  and type variable_type = D.variable_type
  and type link_type = D.link_type
(** Functor building a type-safe version of Pajé trace-recording
    functions. Definitions from the passed module are recorded at the
    "TRACE" level. *)
