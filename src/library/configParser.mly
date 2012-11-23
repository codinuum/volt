/*
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
 */

%{

type error =
  | Invalid_file_contents
  | Invalid_logger_name of string
  | Invalid_logger
  | Invalid_property

let string_of_error = function
  | Invalid_file_contents -> "invalid file contents"
  | Invalid_logger_name x -> Printf.sprintf "invalid logger name %S" x
  | Invalid_logger -> "invalid logger"
  | Invalid_property -> "invalid property"

let fail error =
  let pos = Parsing.symbol_start_pos () in
  let line = pos.Lexing.pos_lnum in
  raise (Configuration.Exception (line, string_of_error error))

%}

%token CLOSING_BRACE EQUAL OPENING_BRACE
%token SEMICOLON LOGGER EOF
%token AND OR
%token <string> IDENT
%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING

%left OR
%left AND

%start file
%type <Configuration.t> file

%%

file: logger_list EOF            { List.rev $1 }
| error                          { fail Invalid_file_contents }

logger_list: /* epsilon */       { [] }
| logger_list logger             { $2 :: $1 }

logger: LOGGER STRING OPENING_BRACE property_list CLOSING_BRACE separator_opt
                                 { let name =
                                   try
                                     Name.of_string $2
                                   with _ -> fail (Invalid_logger_name $2) in
                                   { Configuration.name = name;
                                     Configuration.elements = $4; } }
| LOGGER error                   { fail Invalid_logger }

property_list: /* epsilon */     { [] }
| property_list property         { $2 :: $1 }

property: IDENT EQUAL property_value separator_opt
                                 { ($1, $3) }
| error                          { fail Invalid_property }

property_value: expression       { $1 }
| INTEGER                        { Configuration.Integer $1 }
| FLOAT                          { Configuration.Float $1 }
| STRING                         { Configuration.String $1 }

expression: IDENT                { Configuration.Identifier $1 }
| expression AND expression      { Configuration.And ($1, $3) }
| expression OR expression       { Configuration.Or ($1, $3)  }

separator_opt: /* epsilon */     { }
| SEMICOLON                      { }

%%
