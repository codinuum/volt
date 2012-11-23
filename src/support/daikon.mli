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

(** This module provides support for the Daikon tool.

    Daikon (available at {i http://groups.csail.mit.edu/pag/daikon/}) is
    an invariant detector that works by analyzing the traces produced by
    an instrumented program.

    Bolt can produce Daikon-comptabile traces by using statements like:
    {C [LOG Daikon.t WITH Daikon.point "pt" [Daikon.int "i" i];] }
    more information can be found in the Bolt manual. *)


(** {6 Definitions} *)

val t : string
(** The identifier message for Daikon traces. *)

type variable
(** The type of variables to be recorded in a trace. *)

type properties = (string * string) list
(** The type of information to be recorded in a trace. *)

type 'a variable_builder = string -> 'a -> variable
(** The type of function building variables, taking two parameters:
    - the name of the variable;
    - the value of the variable. *)


(** {6 Variable constructors} *)

val bool : bool variable_builder
(** Defines a variable from name and value. *)

val bool_option : bool option variable_builder
(** Defines a variable from name and value (translated to a Daikon array). *)

val bool_list : bool list variable_builder
(** Defines a variable from name and value (translated to a Daikon array). *)

val bool_array : bool array variable_builder
(** Defines a variable from name and value. *)

val int : int variable_builder
(** Defines a variable from name and value. *)

val int_option : int option variable_builder
(** Defines a variable from name and value (translated to a Daikon array). *)

val int_list : int list variable_builder
(** Defines a variable from name and value (translated to a Daikon array). *)

val int_array : int array variable_builder
(** Defines a variable from name and value. *)

val float : float variable_builder
(** Defines a variable from name and value. *)

val float_option : float option variable_builder
(** Defines a variable from name and value (translated to a Daikon array). *)

val float_list : float list variable_builder
(** Defines a variable from name and value (translated to a Daikon array). *)

val float_array : float array variable_builder
(** Defines a variable from name and value. *)

val string : string variable_builder
(** Defines a variable from name and value. *)

val string_option : string option variable_builder
(** Defines a variable from name and value (translated to a Daikon array). *)

val string_list : string list variable_builder
(** Defines a variable from name and value (translated to a Daikon array). *)

val string_array : string array variable_builder
(** Defines a variable from name and value. *)


(** {6 Variable combinators} *)

val make_variable_builder : ('a -> variable list) -> 'a variable_builder
(** Constructs a variable builder function, by projecting a given value
    to a list of variables. *)

val tuple2 : 'a variable_builder -> 'b variable_builder -> ('a * 'b) variable_builder
(** [tuple2 t1 t2] returns a variable builder for couple of type [t1 * t2]. *)

val tuple3 : 'a variable_builder -> 'b variable_builder -> 'c variable_builder -> ('a * 'b * 'c) variable_builder
(** [tuple3 t1 t2 t3] returns a variable builder for triple of type [t1 * t2 * t3]. *)

val tuple4 : 'a variable_builder -> 'b variable_builder -> 'c variable_builder -> 'd variable_builder -> ('a * 'b * 'c * 'd) variable_builder
(** [tuple4 t1 t2 t3 t4] returns a variable builder for quadruple of type [t1 * t2 * t3 * t4]. *)

val tuple5 : 'a variable_builder -> 'b variable_builder -> 'c variable_builder -> 'd variable_builder -> 'e variable_builder -> ('a * 'b * 'c * 'd * 'e) variable_builder
(** [tuple5 t1 t2 t3 t4 t5] returns a variable builder for quintuple of type [t1 * t2 * t3 * t4 * t5]. *)


(** {6 Properties constructors} *)

val point : string -> variable list -> properties
(** [point id vars] defines the properties for a given point in code,
    [id] being the identifier for the point and [vars] the variable
    list. *)

val enter : string -> variable list -> properties
(** [enter id pars] Defines the properties for a entry point in code
    (typically function begin), [id] being the identifier for the function
    and [pars] the parameter list. *)

val exit : string -> variable -> variable list -> properties
(** [exit id ret pars] Defines the properties for a exit point in code
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

val layout_decls : Layout.t
(** The layout supporting the "Daikon" format (declaration part). *)

val layout_dtrace : Layout.t
(** The layout supporting the "Daikon" format (trace part). *)
