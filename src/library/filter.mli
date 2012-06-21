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

(** This module defines the concept of filter, that is predicate over an event.
    It is possible to register new filters through the [register] function.
    Initially, all the filters defined in this file are registered
    (using the function name as the registration name). *)


(** {6 Definitions} *)

type t = Event.t -> bool
(** The type of event filters.

    A filter should return [true] iff the passed event is of interest. *)

val register : string -> t -> unit
(** [register n f] registers the filter [f] with name [n],
    replacing any existing filter with the same name. *)

val register_unnamed : t -> string
(** Similar to [register] except that an unused name is generated and returned. *)

val get : string -> t
(** [get n] returns the filter registered with name [n].
    Raises [Not_found] if no filter exists with the passed name. *)


(** {6 Trivial filters} *)

val all : t
(** Filter that always returns [true]. *)

val none : t
(** Filter that always returns [false]. *)


(** {6 Time filters} *)

val before : int -> t
(** [before t] returns a filter that returns [true] iff relative time
    of the event is strictly below [t]. *)

val after : int -> t
(** [after t] returns a filter that returns [true] iff relative time
    of the event is strictly above [t]. *)


(** {6 Level filters} *)

val trace_or_below : t
(** Filter that returns [true] iff level is [Level.TRACE] or below. *)

val debug_or_below : t
(** Filter that returns [true] iff level is [Level.DEBUG] or below. *)

val info_or_below : t
(** Filter that returns [true] iff level is [Level.INFO] or below. *)

val warn_or_below : t
(** Filter that returns [true] iff level is [Level.WARN] or below. *)

val error_or_below : t
(** Filter that returns [true] iff level is [Level.ERROR] or below. *)

val fatal_or_below : t
(** Filter that returns [true] iff level is [Level.FATAL] or below. *)


(** {6 Logger filters} *)

val logger_equal : string -> t
(** [logger_equal f] returns a filter that returns [true] iff file is equal to [f]. *)

val logger_not_equal : string -> t
(** [logger_not_equal f] returns a filter that returns [true] iff file is different from [f]. *)


(** {6 File filters} *)

val file_defined : t
(** Filter that returns [true] iff file is different from [""], and ["<nofile>"]. *)

val file_undefined : t
(** Filter that returns [true] iff file is equal to [""], or ["<nofile>"]. *)

val file_equal : string -> t
(** [file_equal f] returns a filter that returns [true] iff file is equal to [f]. *)

val file_not_equal : string -> t
(** [file_not_equal f] returns a filter that returns [true] iff file is different from [f]. *)


(** {6 Position filters} *)

val line_defined : t
(** Filter that returns [true] iff line is above [0]. *)

val line_undefined : t
(** Filter that returns [true] iff line is not above [0]. *)

val column_defined : t
(** Filter that returns [true] iff column is above [0]. *)

val column_undefined : t
(** Filter that returns [true] iff column is not above [0]. *)


(** {6 Message filters} *)

val message_defined : t
(** Filter that returns [true] iff message is different from [""]. *)

val message_undefined : t
(** Filter that returns [true] iff message is equal to [""]. *)


(** {6 Property filters} *)

val properties_empty : t
(** Filter that returns [true] iff property list is empty. *)

val properties_not_empty : t
(** Filter that returns [true] iff property list is not empty. *)

val property_defined : string -> t
(** [property_defined n] returns a filter that returns [true] iff the property
    named [n] is defined. *)

val property_undefined : string -> t
(** [property_undefined n] returns a filter that returns [true] iff the property
    named [n] is not defined. *)

val property_equal : string -> string -> t
(** [property_equal n v] returns a filter that returns [true] iff the property
    named [n] is defined and has value [v]. *)

val property_not_equal : string -> string -> t
(** [property_not_equal n v] returns a filter that returns [true] iff the property
    named [n] is defined and has a value different from [v], or is not defined. *)

val property_equal_pred : string -> (string -> bool) -> t
(** [property_equal_pred n p] returns a filter that returns [true] iff the property
    named [n] is defined with value [v], and [p v] equals [true]. *)

val property_not_equal_pred : string -> (string -> bool) -> t
(** [property_not_equal_pred n p] returns a filter that returns [true] iff the property
    named [n] is defined with value [v] and [p v] equals [false], or is not defined. *)


(** {6 Exception filters} *)

val exception_some : t
(** Filter that returns [true] iff error matches [Some _]. *)

val exception_none : t
(** Filter that returns [true] iff error is equal to [None]. *)


(** {6 Combinators over filters} *)

val logand : t -> t -> t
(** Constructs a filter that is the conjunction of the passed ones. *)

val (&&&) : t -> t -> t
(** Shorthand for [logand]. *)

val logor : t -> t -> t
(** Constructs a filter that is the disjunction of the passed ones. *)

val (|||) : t -> t -> t
(** Shorthand for [logor]. *)

val logxor : t -> t -> t
(** Constructs a filter that is the exclusive disjunction of the passed ones. *)

val (^^^) : t -> t -> t
(** Shorthand for [logxor]. *)

val not : t -> t
(** Constructs a filter that is the negation of the passed one. *)
