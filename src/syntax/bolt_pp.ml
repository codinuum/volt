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


module Args = struct
  let level = ref 5

  let logger = ref ""

  let for_pack = ref ""
end

let logger_name modname =
  if !Args.logger <> "" then
    !Args.logger
  else if !Args.for_pack <> "" then
    !Args.for_pack ^ "." ^ modname
  else
    modname

let module_of_file file =
  let basename = Filename.basename file in
  String.capitalize (try Filename.chop_extension basename with _ -> basename)

module Make (Syntax : Camlp4.Sig.Camlp4Syntax) = struct
  open Camlp4.Sig
  include Syntax

  let level_of_string x = (* re-defined here to avoid dependency *)
    let _loc = Loc.ghost in
    match String.uppercase x with
    | "FATAL" -> <:expr< Bolt.Level.FATAL >>, 0
    | "ERROR" -> <:expr< Bolt.Level.ERROR >>, 1
    | "WARN" -> <:expr< Bolt.Level.WARN >>, 2
    | "INFO" -> <:expr< Bolt.Level.INFO >>, 3
    | "DEBUG" -> <:expr< Bolt.Level.DEBUG >>, 4
    | "TRACE" -> <:expr< Bolt.Level.TRACE >>, 5
    | _ -> failwith (Printf.sprintf "invalid logging level %S" x)

  let level_code_of_int x = (* re-defined here to avoid dependency *)
    match x with
    | -1 -> Bolt.Level.FATAL
    | 0 -> Bolt.Level.FATAL
    | 1 -> Bolt.Level.ERROR
    | 2 -> Bolt.Level.WARN
    | 3 -> Bolt.Level.INFO
    | 4 -> Bolt.Level.DEBUG
    | 5 -> Bolt.Level.TRACE
    | _ -> failwith (Printf.sprintf "invalid logging level '%d'" x)


  let level_code_of_expr = function
    | <:expr< Bolt.Level.FATAL >> -> Bolt.Level.FATAL
    | <:expr< Bolt.Level.ERROR >> -> Bolt.Level.ERROR
    | <:expr< Bolt.Level.WARN >>  -> Bolt.Level.WARN
    | <:expr< Bolt.Level.INFO >>  -> Bolt.Level.INFO
    | <:expr< Bolt.Level.DEBUG >> -> Bolt.Level.DEBUG
    | <:expr< Bolt.Level.TRACE >> -> Bolt.Level.TRACE
    | _ -> assert false


  let push_context, pop_context, peek_context =
    let stack = (ref [] : string list ref) in
    let push prefix n = stack := (prefix^n) :: !stack in
    let pop() =
      match !stack with
      | _ :: rest -> stack := rest
      | _ -> ()
    in
    let peek() = String.concat "" (List.rev !stack) in
    push, pop, peek

  let push_level, pop_level, peek_level =
    let stack = (ref [] : Bolt.Level.t list ref) in
    let push lc = stack := lc :: !stack in
    let pop() =
      match !stack with
      | _ :: rest -> stack := rest
      | _ -> ()
    in
    let peek() = 
      match !stack with
      | lc :: _ -> lc
      | _ -> level_code_of_int !Args.level
    in
    push, pop, peek

  type attributes = {
      error : Ast.expr option;
      properties : Ast.expr option;
      name : string option;
    }

  let no_attribute = { error = None; properties = None; name = None; }

  let merge x y =
    let m n x y = match x, y with
    | Some _, Some _ -> failwith (n ^ " attribute defined twice")
    | Some _, _ -> x
    | _, Some _ -> y
    | _ -> None in
    { error = m "'exception'" x.error y.error;
      properties = m "'properties'" x.properties y.properties;
      name = m "'name'" x.name y.name; }

  let location loc attrs =
    let file = Loc.file_name loc in
    let logger = match attrs.name with
    | Some s -> s
    | None -> (logger_name (module_of_file file))^(peek_context()) in
    let line = string_of_int (Loc.start_line loc) in
    let column = string_of_int ((Loc.start_off loc) - (Loc.start_bol loc)) in
    logger, file, line, column

  let open_bolt _loc e =
    <:expr< (let open Bolt in $e$) >>

  let printf_expr _loc e =
    let error () =
      raise (Stream.Error "constant string or module item expected after \"LOG\"") 
    in
    match e with
    | Ast.ExApp _ ->
        let rec insert x = function
          | Ast.ExApp (loc, e1, e2) ->
              Ast.ExApp (loc, (insert x e1), e2)
          | v -> Ast.ExApp (Ast.loc_of_expr v, x, v) in
        insert (<:expr< Printf.sprintf >>) e
    | Ast.ExStr _ -> e
    | Ast.ExId (_, id) ->
        let rec check_id last = function
          | Ast.IdAcc (_, e1, e2) ->
              check_id false e1;
              check_id true e2
          | Ast.IdLid _ ->
              if not last then error ()
          | Ast.IdUid _ ->
              if last then error ()
          | _ -> error () in
        check_id true id;
        open_bolt _loc e
    | Ast.ExAcc (_, _, _) -> open_bolt _loc e
    | _ -> error ()

  let _transl transl_ _loc es l lvl =
    let attrs = List.fold_left merge no_attribute l in
    let level_code, level_value = level_of_string lvl in
    if level_value <= !Args.level then begin
      let logger, file, line, column = location _loc attrs in
      let properties = match attrs.properties with
      | Some x -> open_bolt _loc x
      | None -> <:expr< [] >> in
      let error = match attrs.error with
      | Some x -> <:expr< Some $x$ >>
      | None -> <:expr< None >> in
      transl_ _loc es logger level_code file line column properties error
    end 
    else
      <:expr< () >>


  let optimize_check _loc level_code need_check no_check =
    let lc = level_code_of_expr level_code in
    let current_lc = peek_level() in
    if lc < current_lc then
      need_check
    else if lc = current_lc then
      no_check
    else
      <:expr< () >>

  let _transl_o _loc es logger level_code file line column properties error =
    match es with
    | [e] ->
	let log =
	  <:expr<
	  Bolt.Logger.log
	    $str:logger$
	    $level_code$
	    ~file:$str:file$
	    ~line:$int:line$
	    ~column:$int:column$
	    ~properties:$properties$
	    ~error:$error$
	  >>
	in
	let msg = printf_expr _loc e in
	optimize_check _loc level_code
	  <:expr< 
	  (if Bolt.Logger.check_level $str:logger$ $level_code$ then ($log$ ($msg$)))
	  >>
	  <:expr< ($log$ $msg$) >>

    | _ -> assert false

  let transl = _transl _transl_o


  let _transl_f _loc es logger level_code file line column properties error =
    match es with
    | [Ast.ExId _] -> _transl_o _loc es logger level_code file line column properties error
    | _ ->
	let logf =
	  <:expr<
	  Bolt.Logger.logf
	    $str:logger$
            $level_code$
            ~file:$str:file$
            ~line:$int:line$
            ~column:$int:column$
            ~properties:$properties$
            ~error:$error$ 
	  >>
	in
	let apply_to_list _loc f lst =
	  List.fold_left 
	    (fun f x -> <:expr< $f$ $x$ >>)
	    f lst
	in
	optimize_check _loc level_code
	  <:expr< 
	  (if Bolt.Logger.check_level $str:logger$ $level_code$ then
	    $apply_to_list _loc logf es$)
	  >>
	  <:expr< ($apply_to_list _loc logf es$) >>

  let transl_f = _transl _transl_f


  let push_lv lc =
    let lc' =
      let cur = peek_level() in
      if lc >= cur then cur else lc
    in
    push_level lc'

  let transl_block _loc sq lvl =
    pop_level();
    let level_code, level_value = level_of_string lvl in
    if level_value <= !Args.level then
      let logger, _, _, _ = location _loc no_attribute in
      optimize_check _loc level_code
	<:expr< 
        (if Bolt.Logger.check_level $str:logger$ $level_code$ then begin $seq:sq$ end)
	>>
        <:expr< begin $seq:sq$ end >>
    else
      <:expr< () >>


  let mk_anti ?(c="") n s = "\\$"^n^c^":"^s

  DELETE_RULE Gram str_item: "module"; a_UIDENT; module_binding0 END;
  DELETE_RULE Gram class_declaration: class_info_for_class_expr; class_fun_binding END;
  DELETE_RULE Gram class_name_and_param: "["; comma_type_parameter; "]"; a_LIDENT  END;
  DELETE_RULE Gram class_name_and_param: a_LIDENT END;
  DELETE_RULE Gram module_binding: a_UIDENT; ":"; module_type; "="; module_expr END;


  EXTEND Gram
    GLOBAL: 
      expr str_item class_declaration class_name_and_param class_str_item
      module_binding let_binding;
    attr: [[ ["EXCEPTION" | "EXN"]; e = expr ->
               { no_attribute with error = Some e }
           | "NAME"; s = STRING ->
               { no_attribute with name = Some s }
           | ["PROPERTIES" | "WITH"]; e = expr ->
               { no_attribute with properties = Some e } ]];

    begin_fatal:
          [[ "BEGIN_FATAL" -> push_lv Bolt.Level.FATAL ]];
    begin_error:
          [[ "BEGIN_ERROR" -> push_lv Bolt.Level.ERROR ]];
    begin_warn:
          [[ "BEGIN_WARN" -> push_lv Bolt.Level.WARN ]];
    begin_info:
          [[ "BEGIN_INFO" -> push_lv Bolt.Level.INFO ]];
    begin_debug:
          [[ "BEGIN_DEBUG" -> push_lv Bolt.Level.DEBUG ]];
    begin_trace:
          [[ "BEGIN_TRACE" -> push_lv Bolt.Level.TRACE ]];

    expr: LEVEL "simple" 
          [[ "LOG"; e = expr; l = LIST0 attr; "LEVEL"; lvl = UIDENT ->
	    transl _loc [e] l lvl
           | "FATAL_MSG" ; es = LIST1 expr LEVEL "."; l = LIST0 attr ->
	     transl_f _loc es l "FATAL"
           | "ERROR_MSG" ; es = LIST1 expr LEVEL "."; l = LIST0 attr ->
	     transl_f _loc es l "ERROR"
           | "WARN_MSG" ; es = LIST1 expr LEVEL "."; l = LIST0 attr ->
	     transl_f _loc es l "WARN"
           | "INFO_MSG" ; es = LIST1 expr LEVEL "."; l = LIST0 attr ->
	     transl_f _loc es l "INFO"
           | "DEBUG_MSG" ; es = LIST1 expr LEVEL "."; l = LIST0 attr ->
	     transl_f _loc es l "DEBUG"
           | "TRACE_MSG" ; es = LIST1 expr LEVEL "."; l = LIST0 attr ->
	     transl_f _loc es l "TRACE"

	   | begin_fatal; sq = sequence; "END_FATAL" -> transl_block _loc sq "FATAL"
	   | begin_fatal; "END_FATAL" -> pop_level(); <:expr< () >>
	   | begin_error; sq = sequence; "END_ERROR" -> transl_block _loc sq "ERROR"
	   | begin_error; "END_ERROR" -> pop_level(); <:expr< () >>
	   | begin_warn; sq = sequence; "END_WARN" -> transl_block _loc sq "WARN"
	   | begin_warn; "END_WARN" -> pop_level(); <:expr< () >>
	   | begin_info; sq = sequence; "END_INFO" -> transl_block _loc sq "INFO"
	   | begin_info; "END_INFO" -> pop_level(); <:expr< () >>
	   | begin_debug; sq = sequence; "END_DEBUG" -> transl_block _loc sq "DEBUG"
	   | begin_debug; "END_DEBUG" -> pop_level(); <:expr< () >>
	   | begin_trace; sq = sequence; "END_TRACE" -> transl_block _loc sq "TRACE"
	   | begin_trace; "END_TRACE" -> pop_level(); <:expr< () >>
      
      ]];
    module_ident:
	  [[ i = a_UIDENT ->
	    push_context "." i;
	    i
	   ]];

    module_binding_head:
	  [ LEFTA [ m = a_UIDENT; ":"; mt = module_type; "=" ->
	    push_context "." m;
	    m, mt
	   ]];

    module_binding:
	  [ LEFTA [ (m, mt) = module_binding_head; me = module_expr ->
	    pop_context();
	    <:module_binding< $m$ : $mt$ = $me$ >>
	   ]];

    str_item: LEVEL "top"
          [[ "module"; i = module_ident; mb = module_binding0 ->
	    pop_context(); 
	    <:str_item< module $i$ = $mb$ >>
	   ]];

    let_binding_ipatt:
	  [[ p = ipatt ->
	    let pushed = 
	      match p with
	      | <:patt< $lid:i$ >> -> push_context "." i; true
	      | _ -> false
	    in
	    p, pushed
	   ]];

    let_binding:
	  [[ (p, pushed) = let_binding_ipatt; e = fun_binding ->
	    if pushed then pop_context();
	    <:binding< $p$ = $e$ >>
	   ]];

    class_name_and_param:
          [[ "["; x = comma_type_parameter; "]"; i = a_LIDENT -> 
	    push_context "." i;
	    (i, x)
           | i = a_LIDENT ->
	    push_context "." i;
	    (i, <:ctyp<>>)
           ]];

    class_declaration:
          [ LEFTA [ ci = class_info_for_class_expr; ce = class_fun_binding ->
	    pop_context();
	    <:class_expr< $ci$ = $ce$ >>
	   ]];

    method_opt_override:
          [[ "method"; "!" -> <:override_flag< ! >>
           | "method"; `ANTIQUOT(("!"|"override"|"anti") as n, s) -> Ast.OvAnt (mk_anti n s)
	   | "method" -> <:override_flag<>>
	   ]];

    method_label:
          [[ l = label ->
	    push_context "#" l;
	    l
	   ]];

    class_str_item:
	  [ LEFTA [ 
	    o = method_opt_override; pf = opt_private; l = method_label; topt = opt_polyt; e = fun_binding ->
	    pop_context();
	    <:class_str_item< method $override:o$ $private:pf$ $l$ : $topt$ = $e$ >>

           | o = method_opt_override; "virtual"; pf = opt_private; l = label; ":"; t = poly_type ->
	    if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< method virtual $private:pf$ $l$ : $t$ >>

	   | o = method_opt_override; pf = opt_private; "virtual"; l = label; ":"; t = poly_type ->
	    if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< method virtual $private:pf$ $l$ : $t$ >>

	   ]];

  END
end

let add_prepare si =
  if !Args.level >= 0 then begin
    let open Camlp4.PreCast in
    let loc = Ast.loc_of_str_item si in
    let file = Loc.file_name loc in
    let logger = logger_name (module_of_file file) in
    let _loc = Loc.ghost in
    let s = <:str_item< let () = Bolt.Logger.prepare $str:logger$ >> in
    Ast.StSem (Loc.ghost, s, si)
  end else
    si

let () =
  let open Camlp4 in
  let module Id = struct let name = "Bolt" let version = "1.4" end in
  let module M = Register.OCamlSyntaxExtension (Id) (Make) in
  PreCast.AstFilters.register_str_item_filter add_prepare;
  let levels = [
    "NONE", -1;
    "FATAL", 0;
    "ERROR", 1;
    "WARN", 2;
    "INFO", 3;
    "DEBUG", 4;
    "TRACE", 5
  ] in
  Options.add
    "-level"
    (Arg.Symbol ((List.map fst levels), (fun s -> Args.level := List.assoc s levels)))
    "<level>  Set the logging level";
  Options.add "-logger" (Arg.Set_string Args.logger) "<name>  Set the logger name";
  Options.add "-for-pack" (Arg.Set_string Args.for_pack) "<prefix>  Set the prefix for logger names"
