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

(** This module defines the concept of layout, that is how an event is
    rendered into a string. It is possible to register new layouts
    through the [register] function. Initially, all the layouts defined
    in this file (except [pattern] and [csv]) are registered (using the
    function name as the registration name).  *)


(** {6 Definitions} *)

type t = (string list) * (string list) * (Event.t -> string)
(** The type of layouts, the components being:
    - the header written before any event (and possibly at each rotation,
      if any);
    - the footer written after all event (and possibly at each rotation,
      if any);
    - the function actually converting the event into a string.

    However, the exact semantics of each element is highly
    output-dependent. *)

val register : string -> t -> unit
(** [register n l] registers the layout [l] with name [n],
    replacing any existing layout with the same name. *)

val register_unnamed : t -> string
(** Similar to [register] except that an unused name is generated and returned. *)

val get : string -> t
(** [get n] returns the layout registered with name [n].

    Raises [Not_found] if no layout exists with the passed name. *)


(** {6 Predefined layouts} *)

val minimal : t
(** The layout initially registered with the name "minimal".

    Format: {v MESSAGE v} *)

val simple : t
(** The layout initially registered with the name "simple".

    Format: {v LEVEL - MESSAGE v} *)

val default : t
(** The layout initially registered with the name "default".

    Format: {v TIME [FILE LINE] LEVEL MESSAGE v} *)

val pattern : string list -> string list -> string -> t
(** [pattern h f r] constructs a layout [(h, f, r')] where [r'] is the
    rendering function defined by the string [r]. The rendering is done
    by substituting the substring of the form ["$(key)"] or
    ["$(key:pad)"] with their associated values. The key/value
    associations are as defined by the function [Event.bindings]. The
    absolute value of pad defines the minimum size of the value when
    rendered. Moreover, if pad is negative padding spaces are added on
    the left while they are added to the right if pad is positive. *)

val html : t
(** The layout initially registered with the name "html".
    Renders the log events into an html table. *)

val csv : string -> string list -> t
(** [csv sep l] constructs a layout [([], [], f)] where [f] is the
    rendering function defined by:
    - [sep] the CSV separator;
    - [l] the list of elements (referred to through keys) to output for
      each CSV record, (the available keys are those defined by
      [Event.bindings]). *)
