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

open Ocamlbuild_plugin

let odocl_file = Pathname.pwd / "bolt.odocl"
let mlpack_file = Pathname.pwd / "bolt.mlpack"
let src_path = Pathname.pwd / "src" / "library"
let sup_path = Pathname.pwd / "src" / "support"

let ignore_modules = [
  "BoltThread"
]

let () =
  let odocl_chan = open_out odocl_file in
  let mlpack_chan = open_out mlpack_file in
  let filenames =
    Array.append
      (Pathname.readdir src_path)
      (Pathname.readdir sup_path) in
  Array.iter
    (fun filename ->
      if (Pathname.check_extension filename "mli")
    || (Pathname.check_extension filename "mly")
    || (Pathname.check_extension filename "mll") then begin
        let modulename = Pathname.remove_extension filename in
        let modulename = Pathname.basename modulename in
        let modulename = String.capitalize_ascii modulename in
        output_string odocl_chan modulename;
        output_char odocl_chan '\n';
        if not (List.mem modulename ignore_modules) then begin
          output_string mlpack_chan modulename;
          output_char mlpack_chan '\n'
        end
      end)
      filenames;
  close_out_noerr odocl_chan;
  close_out_noerr mlpack_chan

let version_tag = "src_library_version_ml"
let version_ml = "src/library/version.ml"
let version_file = "../VERSION"

let () =
  let safe_cp src dst =
    let src = Pathname.mk src in
    let dst = Pathname.mk dst in
    let dir = Pathname.dirname dst in
    let cmd = Printf.sprintf "mkdir -p %s" (Pathname.to_string dir) in
    if Sys.command cmd <> 0 then failwith ("cannot run " ^ cmd);
    cp src dst in
  dispatch begin function
    | After_rules ->
        let camlp4of =
          try
            let path_bin = Filename.concat (Sys.getenv "PATH_OCAML_PREFIX") "bin" in
            Filename.concat path_bin "camlp4of"
          with _ -> "camlp4of" in
        flag ["ocaml"; "compile"; "pp_camlp4of"] (S[A"-pp"; A camlp4of]);
        flag ["ocaml"; "pp:dep"; "pp_camlp4of"] (S[A camlp4of]);
        if String.uppercase_ascii (try Sys.getenv "WARNINGS" with _ -> "") = "TRUE" then
          flag ["ocaml"; "compile"; "warnings"] (S[A"-w"; A"Ae"; A"-warn-error"; A"A"]);
        dep [version_tag] [version_ml];
        rule ("generation of " ^ version_ml)
          ~prod:version_ml
          ~insert:`bottom
          (fun _ _ ->
            let version =
              try
                List.hd (string_list_of_file (Pathname.mk version_file))
              with _ -> "unknown" in
            let name, channel = Filename.open_temp_file "version" ".ml" in
            Printf.fprintf channel "let value = %S\n" version;
            close_out_noerr channel;
            safe_cp name version_ml);
    | _ -> ()
  end
