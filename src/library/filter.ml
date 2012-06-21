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


(* Definitions *)

type t = Event.t -> bool

let filters, register, register_unnamed, get =
  Utils.make_container_functions ()


(* Trivial filters *)

let all _ = true

let none _ = false


(* Time filters *)

let before t = fun e -> e.Event.relative < t

let after t = fun e -> e.Event.relative > t


(* Level filters *)

let trace_or_below e = e.Event.level <= Level.TRACE

let debug_or_below e = e.Event.level <= Level.DEBUG

let info_or_below e = e.Event.level <= Level.INFO

let warn_or_below e = e.Event.level <= Level.WARN

let error_or_below e = e.Event.level <= Level.ERROR

let fatal_or_below e = e.Event.level <= Level.FATAL


(* Logger filters *)

let logger_equal f = fun e -> e.Event.logger = f

let logger_not_equal f = fun e -> e.Event.logger <> f


(** {6 File filters} *)

let file_defined e = e.Event.file <> "" && e.Event.file <> "<nofile>"

let file_undefined e = e.Event.file = "" || e.Event.file = "<nofile>"

let file_equal f = fun e -> e.Event.file = f

let file_not_equal f = fun e -> e.Event.file = f


(* Position filters *)

let line_defined e = e.Event.line > 0

let line_undefined e = e.Event.line <= 0

let column_defined e = e.Event.column > 0

let column_undefined e = e.Event.column <= 0


(* Message filters *)

let message_defined e = e.Event.message <> ""

let message_undefined e = e.Event.message = ""


(* Property filters *)

let properties_empty e = e.Event.properties = []

let properties_not_empty e = e.Event.properties <> []

let property_defined k = fun e -> List.mem_assoc k e.Event.properties

let property_undefined k = fun e -> not (List.mem_assoc k e.Event.properties)

let property_equal k v = fun e -> try (List.assoc k e.Event.properties) = v with Not_found -> false

let property_not_equal k v = fun e -> try (List.assoc k e.Event.properties) <> v with Not_found -> true

let property_equal_pred k p = fun e -> try p (List.assoc k e.Event.properties) with Not_found -> false

let property_not_equal_pred k p = fun e -> try not (p (List.assoc k e.Event.properties)) with Not_found -> true


(* Exception filters *)

let exception_some e = match e.Event.error with Some _ -> true | None -> false

let exception_none e = match e.Event.error with Some _ -> false | None -> true


(* Combinators over filters *)

let logand f1 f2 = fun e -> (f1 e) && (f2 e)

let (&&&) = logand

let logor f1 f2 = fun e -> (f1 e) || (f2 e)

let (|||) = logor

let logxor f1 f2 = fun e -> let r1 = f1 e and r2 = f2 e in (r1 && (not r2)) || ((not r1) && r2)

let (^^^) = logxor

let not f = fun e -> not (f e)


let () =
  List.iter
    (fun (x, y) -> register x y)
    [ "all",                  all ;
      "none",                 none ;
      "trace_or_below",       trace_or_below ;
      "debug_or_below",       debug_or_below ;
      "info_or_below",        info_or_below ;
      "warn_or_below",        warn_or_below ;
      "error_or_below",       error_or_below ;
      "fatal_or_below",       fatal_or_below ;
      "file_defined",         file_defined ;
      "file_undefined",       file_undefined ;
      "line_defined",         line_defined ;
      "line_undefined",       line_undefined ;
      "column_defined",       column_defined ;
      "column_undefined",     column_undefined ;
      "message_defined",      message_defined ;
      "message_undefined",    message_undefined ;
      "properties_empty",     properties_empty ;
      "properties_not_empty", properties_not_empty ;
      "exception_some",       exception_some ;
      "exception_none",       exception_none ]
