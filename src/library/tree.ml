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


type logger_info = {
    name : Name.t;
    level : Level.t;
    filter : Filter.t lazy_t;
    pass : Filter.t lazy_t;
    layout : Layout.t lazy_t;
    mode : Mode.t;
    output : Output.impl lazy_t;
  }

module StringMap = Map.Make (String)

type node = {
    parent : node option;
    path : Name.t;
    mutable loggers : logger_info list;
    mutable children : node StringMap.t;
  }

let root =
  { parent = None;
    path = Name.of_list [];
    loggers = [];
    children = StringMap.empty; }

let close_all () =
  let rec down node =
    List.iter
      (fun x ->
        try
          let o = Lazy.force x.output in
          x.mode#flush o;
          o#close
        with _ -> ())
      node.loggers;
    StringMap.iter
      (fun _ v -> down v)
      node.children in
  down root

let () = at_exit close_all

let rec get_node name_elements path current_node =
  match name_elements with
  | hd :: tl ->
      let path = hd :: path in
      (try
        get_node tl path (StringMap.find hd current_node.children)
      with Not_found ->
        let new_node =
          { parent = Some current_node;
            path = Name.of_list (List.rev path);
            loggers = [];
            children = StringMap.empty; } in
        current_node.children <- StringMap.add hd new_node current_node.children;
        get_node tl path new_node)
  | [] -> current_node

let get_node name =
  get_node (Name.to_list name) [] root

let register_logger info =
  Utils.enter_critical_section ();
  try
    let node = get_node info.name in
    node.loggers <- info :: node.loggers;
    Utils.leave_critical_section ()
  with e ->
    Utils.leave_critical_section ();
    raise e

let get_loggers name =
  let rec up node acc =
    let acc = (node.path, node.loggers) :: acc in
    match node.parent with
    | Some p -> up p acc
    | None -> List.rev acc in
  Utils.enter_critical_section ();
  try
    let node = get_node name in
    let res = up node [] in
    Utils.leave_critical_section ();
    res
  with e ->
    Utils.leave_critical_section ();
    raise e

let make_node name =
  Utils.enter_critical_section ();
  try
    ignore (get_node name);
    Utils.leave_critical_section ()
  with e ->
    Utils.leave_critical_section ();
    raise e

