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

(* Definitions *)

let magic_trace = "Paje trace\000(magic\001)"

let magic_kind = "Paje kind\000(magic\001)"

let t = magic_trace

let () = Utils.paje_t := t

type properties = (string * string) list

type name = string

type alias = string

type color = float * float * float

let string_of_color (r, g, b) =
  Printf.sprintf "%f %f %f" r g b

type field_type =
  | Date
(*  | Int not used *)
  | Double
(*  | Hex not used *)
  | String
  | Color

let string_of_field_type = function
  | Date -> "date"
(*  | Int -> "int" not used *)
  | Double -> "double"
(*  | Hex -> "hex" not used *)
  | String -> "string"
  | Color -> "color"

type event = {
    event_name : string;
    event_id : int;
    event_fields : (string * field_type) list;
  }
(* The type of event definitions. *)

let string_of_event ev =
  let field_def (nam, typ) =
    Printf.sprintf "%%\t%s\t%s" nam (string_of_field_type typ) in
  ((Printf.sprintf "%%EventDef\t%s\t%d" ev.event_name ev.event_id)
   :: (List.map field_def ev.event_fields))
  @ ["%EndEventDef"]

let event_def =
  let id = ref 0 in
  fun name l ->
    incr id;
    { event_name = name;
      event_id = !id;
      event_fields = l; }

let predefined_events = [
  event_def "PajeDefineContainerType"
    ["Name", String; "Type", String; "Alias", String];
  event_def "PajeDefineStateType"
    ["Name", String; "Type", String; "Alias", String];
  event_def "PajeDefineEventType"
    ["Name", String; "Type", String; "Alias", String];
  event_def "PajeDefineVariableType"
    ["Name", String; "Type", String; "Color", Color; "Alias", String];
  event_def "PajeDefineLinkType"
    ["Name", String; "Type", String; "StartContainerType", String; "EndContainerType", String; "Alias", String];
  event_def "PajeDefineEntityValue"
    ["Name", String; "Type", String; "Color", Color; "Alias", String];
  event_def "PajeCreateContainer"
    ["Time", Date; "Name", String; "Type", String; "Container", String; "Alias", String];
  event_def "PajeDestroyContainer"
    ["Time", Date; "Name", String; "Type", String];
  event_def "PajeSetState"
    ["Time", Date; "Type", String; "Container", String; "Value", String];
  event_def "PajePushState"
    ["Time", Date; "Type", String; "Container", String; "Value", String];
  event_def "PajePopState"
    ["Time", Date; "Type", String; "Container", String];
  event_def "PajeResetState"
    ["Time", Date; "Type", String; "Container", String];
  event_def "PajeNewEvent"
    ["Time", Date; "Type", String; "Container", String; "Value", String];
  event_def "PajeSetVariable"
    ["Time", Date; "Type", String; "Container", String; "Value", Double];
  event_def "PajeAddVariable"
    ["Time", Date; "Type", String; "Container", String; "Value", Double];
  event_def "PajeSubVariable"
    ["Time", Date; "Type", String; "Container", String; "Value", Double];
  event_def "PajeStartLink"
    ["Time", Date; "Type", String; "Container", String; "StartContainer", String; "Value", String; "Key", String];
  event_def "PajeEndLink"
    ["Time", Date; "Type", String; "Container", String; "EndContainer", String; "Value", String; "Key", String]
]

(* Predefined events: type definitions *)

let define_container_type ~name ?(typ="0") ?(alias=name) l =
  [magic_kind, "PajeDefineContainerType";
   "Name", name;
   "Type", typ;
   "Alias", alias]
  @ l

let define_state_type ~name ~typ ?(alias=name) l =
  [magic_kind, "PajeDefineStateType";
   "Name", name;
   "Type", typ;
   "Alias", alias]
  @ l

let define_event_type ~name ~typ ?(alias=name) l =
  [magic_kind, "PajeDefineEventType";
   "Name", name;
   "Type", typ;
   "Alias", alias]
  @ l

let define_variable_type ~name ~typ ~color ?(alias=name) l =
  [magic_kind, "PajeDefineVariableType";
   "Name", name;
   "Type", typ;
   "Color", string_of_color color;
   "Alias", alias]
  @ l

let define_link_type ~name ~typ ~start_container_type ~end_container_type ?(alias=name) l =
  [magic_kind, "PajeDefineLinkType";
   "Name", name;
   "Type", typ;
   "StartContainerType", start_container_type;
   "EndContainerType", end_container_type;
   "Alias", alias]
  @ l

let define_entity_value ~name ~typ ~color ?(alias=name) l =
  [magic_kind, "PajeDefineEntityValue";
   "Name", name;
   "Type", typ;
   "Color", string_of_color color;
   "Alias", alias]
  @ l


(* Predefined events: trace recording *)

let create_container ~name ~typ ?(container="0") ?(alias=name) l =
  [magic_kind, "PajeCreateContainer";
   "Name", name;
   "Alias", alias;
   "Type", typ;
   "Container", container]
  @ l

let destroy_container ~name ~typ l =
  [magic_kind, "PajeDestroyContainer";
   "Name", name;
   "Type", typ]
  @ l

let set_state ~typ ~container ~value l =
  [magic_kind, "PajeSetState";
   "Type", typ;
   "Container", container;
   "Value", value]
  @ l

let push_state ~typ ~container ~value l =
  [magic_kind, "PajePushState";
   "Type", typ;
   "Container", container;
   "Value", value]
  @ l

let pop_state ~typ ~container l =
  [magic_kind, "PajePopState";
   "Type", typ;
   "Container", container]
  @ l

let reset_state ~typ ~container l =
  [magic_kind, "PajeResetState";
   "Type", typ;
   "Container", container]
  @ l

let new_event ~typ ~container ~value l =
  [magic_kind, "PajeNewEvent";
   "Type", typ;
   "Container", container;
   "Value", value]
  @ l

let set_variable ~typ ~container ~value l =
  [magic_kind, "PajeSetVariable";
   "Type", typ;
   "Container", container;
   "Value", string_of_float value]
  @ l

let add_variable ~typ ~container ~value l =
  [magic_kind, "PajeAddVariable";
   "Type", typ;
   "Container", container;
   "Value", string_of_float value]
  @ l

let sub_variable ~typ ~container ~value l =
  [magic_kind, "PajeSubVariable";
   "Type", typ;
   "Container", container;
   "Value", string_of_float value]
  @ l

let start_link ~typ ~container ~start_container ~value ~key l =
  [magic_kind, "PajeStartLink";
   "Type", typ;
   "Container", container;
   "StartContainer", start_container;
   "Value", value;
   "Key", key]
  @ l

let end_link ~typ ~container ~end_container ~value ~key l =
  [magic_kind, "PajeEndLink";
   "Type", typ;
   "Container", container;
   "EndContainer", end_container;
   "Value", value;
   "Key", key]
  @ l


(* Layout elements *)

let extract_kind l = List.partition (fun (x, _) -> x = magic_kind) l

let render e =
  let default = function
    | Date -> string_of_float ((float e.Event.relative) /. 1000.)
(*    | Int -> "0" not used *)
    | Double -> "0.0"
(*    | Hex -> "00" not used *)
    | String -> "\"\""
    | Color -> "1.0 1.0 1.0" in
  let make_fields id fields properties =
    let res =
      List.map
        (fun (nam, typ) ->
          try
            let res = List.assoc nam properties in
            if typ = String then Printf.sprintf "%S" res else res
          with Not_found -> default typ)
        fields in
    let res = (string_of_int id) :: res in
    String.concat " " res in
  if e.Event.message = magic_trace then
    try
      let kind, rest = extract_kind e.Event.properties in
      match kind with
      | [_, kind] ->
          let def =
            List.find
              (fun ed -> kind = ed.event_name)
              predefined_events in
          make_fields def.event_id def.event_fields rest
      | _ -> ""
    with _ -> ""
  else
    ""

let header = List.concat (List.map string_of_event predefined_events)

let layout = header, [], render

let layout_noheader = [], [], render

let () =
  List.iter
    (fun (x, y) -> Layout.register x y)
    [ "paje",          layout ;
      "paje_noheader", layout_noheader ]


(* Functorial interface *)

type type_kind = Container | State | Event | Variable | Link | Entity_value

let string_of_type_kind = function
  | Container -> "container"
  | State -> "state"
  | Event -> "event"
  | Variable -> "variable"
  | Link -> "link"
  | Entity_value -> "entity value"

exception Invalid_type of type_kind

let () =
  Printexc.register_printer
    (function
      | Invalid_type tk ->
          let msg = Printf.sprintf "Invalid Pajé %s" (string_of_type_kind tk) in
          Some msg
      | _ -> None)

module type Definitions = sig
  val logger : string
  val level : Level.t
  type container_type
  val container_types : (container_type * name * (container_type option) * alias) list
  type state_type
  val state_types : (state_type * name * container_type * alias) list
  type event_type
  val event_types : (event_type * name * container_type * alias) list
  type variable_type
  val variable_types : (variable_type * name * container_type * color * alias) list
  type link_type
  val link_types : (link_type * name * container_type * container_type * container_type * alias) list
  type entity_value_type
  val entity_value_types : (entity_value_type * name * container_type * color * alias) list
end

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

module Make (D : Definitions) = struct
  let t = t
  type properties = (string * string) list
  type name = string
  type alias = string
  type color = float * float * float
  type container_type = D.container_type
  type state_type = D.state_type
  type event_type = D.event_type
  type variable_type = D.variable_type
  type link_type = D.link_type
  type entity_value_type = D.entity_value_type

  let make tk =
    let h = Hashtbl.create 17 in
    let g x =
      try
        Hashtbl.find h x
      with Not_found ->
        raise (Invalid_type tk) in
    h, g

  let containers, get_container = make Container
  let states, get_state = make State
  let events, get_event = make Event
  let variables, get_variable = make Variable
  let links, get_link = make Link
  let entity_values, _ = make Entity_value

  let () =
    (* containers *)
    List.iter
      (fun (x, name, parent, alias) ->
        Hashtbl.add containers x name;
        let typ = match parent with
        | Some x ->
            (try
              Hashtbl.find containers x
            with _ -> "0")
        | None -> "0" in
        let properties = define_container_type ~name ~typ ~alias [] in
        Logger.log D.logger D.level ~properties t)
      D.container_types;
    (* states *)
    List.iter
      (fun (x, name, typ, alias) ->
        Hashtbl.add states x name;
        let typ = get_container typ in
        let properties = define_state_type ~name ~typ ~alias [] in
        Logger.log D.logger D.level ~properties t)
      D.state_types;
    (* events *)
    List.iter
      (fun (x, name, typ, alias) ->
        Hashtbl.add events x name;
        let typ = get_container typ in
        let properties = define_event_type ~name ~typ ~alias [] in
        Logger.log D.logger D.level ~properties t)
      D.event_types;
    (* variables *)
    List.iter
      (fun (x, name, typ, color, alias) ->
        Hashtbl.add variables x name;
        let typ = get_container typ in
        let properties = define_variable_type ~name ~typ ~color ~alias [] in
        Logger.log D.logger D.level ~properties t)
      D.variable_types;
    (* links *)
    List.iter
      (fun (x, name, typ, start_container_type, end_container_type, alias) ->
        Hashtbl.add links x name;
        let typ = get_container typ in
        let start_container_type = get_container start_container_type in
        let end_container_type = get_container end_container_type in
        let properties = define_link_type ~name ~typ ~start_container_type ~end_container_type ~alias [] in
        Logger.log D.logger D.level ~properties t)
      D.link_types;
    (* entity values *)
    List.iter
      (fun (x, name, typ, color, alias) ->
        Hashtbl.add entity_values x name;
        let typ = get_container typ in
        let properties = define_entity_value ~name ~typ ~color ~alias [] in
        Logger.log D.logger D.level ~properties t)
      D.entity_value_types

  let create_container ~name ~typ ?(container="0") ?(alias=name) l =
    let typ = get_container typ in
    create_container ~name ~typ ~container ~alias l

  let destroy_container ~name ~typ l =
    let typ = get_container typ in
    destroy_container ~name ~typ l

  let set_state ~typ ~container ~value l =
    let typ = get_state typ in
    set_state ~typ ~container ~value l

  let push_state ~typ ~container ~value l =
    let typ = get_state typ in
    push_state ~typ ~container ~value l

  let pop_state ~typ ~container l =
    let typ = get_state typ in
    pop_state ~typ ~container l

  let reset_state ~typ ~container l =
    let typ = get_state typ in
    reset_state ~typ ~container l

  let new_event ~typ ~container ~value l =
    let typ = get_event typ in
    new_event ~typ ~container ~value l

  let set_variable ~typ ~container ~value l =
    let typ = get_variable typ in
    set_variable ~typ ~container ~value l

  let add_variable ~typ ~container ~value l =
    let typ = get_variable typ in
    add_variable ~typ ~container ~value l

  let sub_variable ~typ ~container ~value l =
    let typ = get_variable typ in
    sub_variable ~typ ~container ~value l

  let start_link ~typ ~container ~start_container ~value ~key l =
    let typ = get_link typ in
    start_link ~typ ~container ~start_container ~value ~key l

  let end_link ~typ ~container ~end_container ~value ~key l =
    let typ = get_link typ in
    end_link ~typ ~container ~end_container ~value ~key l

end
