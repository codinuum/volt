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

let header = []

let protect s =
  let len = String.length s in
  let buf = Buffer.create len in
  String.iter
    (fun ch ->
      match ch with
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '\"' -> Buffer.add_string buf "&quot;"
      | '&' -> Buffer.add_string buf "&amp;"
      | _ -> Buffer.add_char buf ch)
    s;
  Buffer.contents buf


let protect_cdata s =
  let len = String.length s in
  let buf = Buffer.create len in
  let i = ref 0 in
  while (!i < len) do
    if (!i >= 2) && (s.[!i - 2] = ']') && (s.[!i - 1] = ']') && (s.[!i] = '>') then
      Buffer.add_string buf " >"
    else
      Buffer.add_char buf s.[!i];
    incr i
  done;
  Buffer.contents buf

let render e =
  let properties =
    List.map
      (fun (k, v) ->
        Printf.sprintf "  <log4j:data name=\"%s\" value=\"%s\"/>\n"
          (protect k)
          (protect v))
      e.Event.properties in
  Event.render_bindings
    (List.map
       (fun (k, v) -> (k, (if k = "message" then protect_cdata v else protect v)))
       (Event.bindings e))
    ("<log4j:event logger=\"$(logger)\" level=\"$(level)\" thread=\"$(thread)\" timestamp=\"$(time)\">\n" ^
     "<log4j:message><![CDATA[$(message)]]></log4j:message>\n" ^
     (match e.Event.error with
     | Some (_, s) -> "<log4j:throwable><![CDATA[" ^ (protect_cdata s) ^ "]]></log4j:throwable>\n"
     | None -> "") ^
     "<log4j:locationInfo class=\"Unknown\" method=\"unknown()\" file=\"$(file)\" line=\"$(line)\"/>\n" ^
     "<log4j:properties>\n" ^
     (String.concat "" properties) ^
     "</log4j:properties>\n" ^
     "</log4j:event>\n")

let layout = header, [], render

let () =
  List.iter
    (fun (x, y) -> Layout.register x y)
    [ "xml",   layout ;
      "log4j", layout ]
