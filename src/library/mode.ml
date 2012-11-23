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


type error =
  | Invalid_condition_string of string

let string_of_error = function
  | Invalid_condition_string s -> Printf.sprintf "invalid condition string %S" s

exception Exception of error

let () =
  Printexc.register_printer
    (function
      | Exception error -> Some (string_of_error error)
      | _ -> None)

let fail error =
  raise (Exception error)

type condition =
  | Events of int
  | Bytes of int
  | Seconds of int

let condition_of_string s =
  let fail () = fail (Invalid_condition_string s) in
  let len = String.length s in
  if len > 1 then begin
    let arg =
      try
        int_of_string (String.sub s 0 (pred len))
      with _ -> fail () in
    match s.[pred len] with
    | 'e' -> Events arg
    | 'b' -> Bytes arg
    | 's' -> Seconds arg
    | _ -> fail ()
  end else
    fail ()

class type t =
  object
    method deliver : Output.impl -> string -> unit
    method flush : Output.impl -> unit
  end

let direct () =
  object
    method deliver o s =
      o#write s
    method flush _ =
      ()
  end

let memory () =
  object
    val buff = Buffer.create 1024
    method deliver _ s =
      if Buffer.length buff > 0 then Buffer.add_char buff '\n';
      Buffer.add_string buff s
    method flush o =
      o#write (Buffer.contents buff);
      Buffer.clear buff
  end

class virtual buffered =
  object (self)
    val buff = Buffer.create 1024
    method virtual update_condition : string -> unit
    method virtual reset_condition : unit
    method virtual condition : bool
    method deliver (o : Output.impl) s =
      self#update_condition s;
      if Buffer.length buff > 0 then Buffer.add_char buff '\n';
      Buffer.add_string buff s;
      if self#condition then self#flush o
    method flush o =
      o#write (Buffer.contents buff);
      Buffer.clear buff;
      self#reset_condition
  end

let retained s =
  let res = match condition_of_string s with
  | Events x ->
      object
        inherit buffered  
        val mutable events = 0
        method update_condition _ = events <- events + 1
        method reset_condition = events <- 0
        method condition = events >= x
      end
  | Bytes x ->
      object
        inherit buffered  
        method update_condition _ = ()
        method reset_condition = ()
        method condition = Buffer.length buff >= x
      end
  | Seconds x ->
      let x = float_of_int x in
      object
        inherit buffered  
        val mutable last = Unix.gettimeofday ()
        method update_condition _ = ()
        method reset_condition = last <- Unix.gettimeofday ()
        method condition = let now = Unix.gettimeofday () in now -. last > x
      end in
  (res :> t)
