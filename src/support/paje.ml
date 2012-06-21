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

let magic_trace = "Paje trace\000(magic\001)"

let magic_kind = "Paje kind\000(magic\001)"

let t = magic_trace

type properties = (string * string) list

type field_type =
  | Date
  | Int
  | Double
  | Hex
  | String

let string_of_field_type = function
  | Date -> "date"
  | Int -> "int"
  | Double -> "double"
  | Hex -> "hex"
  | String -> "string"

type event = {
    event_name : string;
    event_id : int;
    event_fields : (string * field_type) list;
  }

let event_def =
  let id = ref 0 in
  fun name l ->
    incr id;
    { event_name = name;
      event_id = !id;
      event_fields = l; }

let predefined_events = [
  event_def "PajeDefineContainerType"
    ["Alias", String; "ContainerType", String; "Name", String];
  event_def "PajeCreateContainer"
    ["Time", Date; "Alias", String; "Type", String; "Container", String; "Name", String];
  event_def "PajeDestroyContainer"
    ["Time", Date; "Name", String; "Type", String];
  event_def "PajeDefineEventType"
    ["Name", String; "ContainerType", String; "Alias", String];
  event_def "PajeDefineStateType"
    ["Name", String; "ContainerType", String; "Alias", String];
  event_def "PajeDefineVariableType"
    ["Name", String; "ContainerType", String; "Alias", String];
  event_def "PajeDefineLinkType"
    ["Name", String; "ContainerType", String; "SourceContainerType", String; "DestContainerType", String; "Alias", String];
  event_def "PajeDefineEntityValue"
    ["Name", String; "EntityType", String; "Alias", String];
  event_def "PajeNewEvent"
    ["Time", Date; "Type", String; "Container", String; "Value", String];
  event_def "PajeSetState"
    ["Time", Date; "Type", String; "Container", String; "Value", String];
  event_def "PajePushState"
    ["Time", Date; "Type", String; "Container", String; "Value", String];
  event_def "PajePopState"
    ["Time", Date; "Type", String; "Container", String];
  event_def "PajeSetVariable"
    ["Time", Date; "Type", String; "Container", String; "Value", Double];
  event_def "PajeAddVariable"
    ["Time", Date; "Type", String; "Container", String; "Value", Double];
  event_def "PajeSubVariable"
    ["Time", Date; "Type", String; "Container", String; "Value", Double];
  event_def "PajeStartLink"
    ["Time", Date; "Type", String; "Container", String; "SourceContainer", String; "Value", String; "Key", String];
  event_def "PajeEndLink"
    ["Time", Date; "Type", String; "Container", String; "DestContainer", String; "Value", String; "Key", String]
]

(* Predefined events *)

let define_container_type ~name ?(alias=name) ~container_type l =
  [magic_kind, "PajeDefineContainerType";
   "Name", name;
   "Alias", alias;
   "ContainerType", container_type]
  @ l

let create_container ~name ?(alias=name) ~typ ~container l =
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

let define_event_type ~name ?(alias=name) ~container_type l =
  [magic_kind, "PajeDefineEventType";
   "Name", name;
   "Alias", alias;
   "ContainerType", container_type]
  @ l

let define_state_type ~name ?(alias=name) ~container_type l =
  [magic_kind, "PajeDefineStateType";
   "Name", name;
   "Alias", alias;
   "ContainerType", container_type]
  @ l

let define_variable_type ~name ?(alias=name) ~container_type l =
  [magic_kind, "PajeDefineVariableType";
   "Name", name;
   "Alias", alias;
   "ContainerType", container_type]
  @ l

let define_link_type ~name ?(alias=name) ~container_type ~source_container_type ~dest_container_type l =
  [magic_kind, "PajeDefineLinkType";
   "Name", name;
   "Alias", alias;
   "ContainerType", container_type;
   "SourceContainerType", source_container_type;
   "DestContainerType", dest_container_type]
  @ l

let define_entity_value ~name ?(alias=name) ~entity_type l =
  [magic_kind, "PajeDefineEntityValue";
   "Name", name;
   "Alias", alias;
   "EntityType", entity_type]
  @ l

let new_event ~typ ~container ~value l =
  [magic_kind, "PajeNewEvent";
   "Type", typ;
   "Container", container;
   "Value", value]
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

let start_link ~typ ~container ~source_container ~value ~key l =
  [magic_kind, "PajeStartLink";
   "Type", typ;
   "Container", container;
   "SourceContainer", source_container;
   "Value", value;
   "Key", key]
  @ l

let end_link ~typ ~container ~dest_container ~value ~key l =
  [magic_kind, "PajeEndLink";
   "Type", typ;
   "Container", container;
   "DestContainer", dest_container;
   "Value", value;
   "Key", key]
  @ l


(* Layout elements *)

let string_of_event ev =
  let field_def (nam, typ) =
    Printf.sprintf "%%\t%s\t%s" nam (string_of_field_type typ) in
  ((Printf.sprintf "%%EventDef\t%s\t%d" ev.event_name ev.event_id)
   :: (List.map field_def ev.event_fields))
  @ ["%EndEventDef"]

let extract_kind l = List.partition (fun (x, _) -> x = magic_kind) l

let render e =
  let default = function
    | Date -> string_of_float ((float e.Event.relative) /. 1000.)
    | Int -> "0"
    | Double -> "0.0"
    | Hex -> "00"
    | String -> "\"\"" in
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
