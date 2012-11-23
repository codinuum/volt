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


type t = {
    as_string : string;
    as_list : string list;
  }

let normalize_list l =
  List.filter
    (fun s -> s <> "")
    (List.map Utils.trim l)

let of_string s =
  let l = normalize_list (Utils.split "." s) in
  { as_string = String.concat "." l;
    as_list = l; }

let of_list l =
  let l = normalize_list l in
  { as_string = String.concat "." l;
    as_list = l; }

let to_string n =
  n.as_string

let to_list n =
  n.as_list
