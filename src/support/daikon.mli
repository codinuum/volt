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

(** This module provides support for the Daikon tool.

    Daikon (available at {i http://groups.csail.mit.edu/pag/daikon/}) is
    an invariant detector that works by analyzing the traces produced by
    an instrumented program.

    Bolt can produce Daikon-comptabile traces by using statements like:
    {C [LOG Daikon.t WITH Daikon.point "pt" [Daikon.int "i" i];] }
    more information can be found in the Bolt manual.
 *)


(** {6 Definitions} *)

val t : string
(** The identifier message for Daikon traces. *)

type variable
(** The type of variables to be recorded in a trace. *)

type properties = (string * string) list
(** The type of information to be recorded in a trace. *)


(** {6 Variable constructors} *)

val bool : string -> bool -> variable
(** Defines a variable from name and value.*)

val bool_list : string -> bool list -> variable
(** Defines a variable from name and value.*)

val bool_array : string -> bool array -> variable
(** Defines a variable from name and value.*)

val int : string -> int -> variable
(** Defines a variable from name and value.*)

val int_list : string -> int list -> variable
(** Defines a variable from name and value.*)

val int_array : string -> int array -> variable
(** Defines a variable from name and value.*)

val float : string -> float -> variable
(** Defines a variable from name and value.*)

val float_list : string -> float list -> variable
(** Defines a variable from name and value.*)

val float_array : string -> float array -> variable
(** Defines a variable from name and value.*)

val string : string -> string -> variable
(** Defines a variable from name and value.*)

val string_list : string -> string list -> variable
(** Defines a variable from name and value.*)

val string_array : string -> string array -> variable
(** Defines a variable from name and value.*)


(** {6 Properties constructors} *)

val point : string -> variable list -> properties
(** [point id vars] defines the properties for a given point in code,
    [id] being the identifier for the point and [vars] the variable list. *)

val enter : string -> variable list -> properties
(** [point id pars] Defines the properties for a entry point in code
    (typically function begin), [id] being the identifier for the function
    and [pars] the parameter list. *)

val exit : string -> variable -> variable list -> properties
(** [point id ret pars] Defines the properties for a exit point in code
    (typically function end), [id] being the identifier for the function,
    [ret] the returned value and [pars] the parameter list. *)


(** {6 Layout elements} *)

val decls_header : string list
(** The header defining Daikon events (declaration part). *)

val decls_render : Event.t -> string
(** The rendering function for Daikon format (declaration part). *)

val dtrace_header : string list
(** The header defining Daikon events (trace part). *)

val dtrace_render : Event.t -> string
(** The rendering function for Daikon format (trace part). *)
