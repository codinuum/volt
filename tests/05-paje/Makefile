#
# This file is part of Bolt.
# Copyright (C) 2009-2012 Xavier Clerc.
#
# Bolt is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Bolt is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

include ../../Makefile.config

SYNTAX=$(PATH_OCAML_BIN)/camlp4o ../../_build/src/syntax/bolt_pp.cmo -for-pack Pack
OCAMLJAVA=$(PATH_OCAML_BIN)/ocamljava
ifeq ($(findstring $(OCAMLJAVA),$(wildcard $(OCAMLJAVA))),$(OCAMLJAVA))
OCAMLJAVA_AVAILABLE=yes
else
OCAMLJAVA_AVAILABLE=no
endif

default::
	@$(MAKE) -s compile COMPILER=$(PATH_OCAML_BIN)/ocamlc OBJ_EXT=cmo LIB_EXT=cma EXE_EXT=byte EXTRA_ARGS=
	@$(MAKE) -s run EXEC=./main.byte
ifeq ($(NATIVE_DYNLINK),TRUE)
	@$(MAKE) -s compile COMPILER=$(PATH_OCAML_BIN)/ocamlopt OBJ_EXT=cmx LIB_EXT=cmxa EXE_EXT=opt EXTRA_ARGS= || true
	@$(MAKE) -s run EXEC=./main.opt || true
else
endif
ifeq ($(OCAMLJAVA_AVAILABLE),yes)
	@$(MAKE) -s compile COMPILER=$(PATH_OCAML_BIN)/ocamljava OBJ_EXT=cmj LIB_EXT=cmja EXE_EXT=jar EXTRA_ARGS=-standalone
	@$(MAKE) -s run EXEC='java -jar main.jar'
else
endif
	@$(MAKE) -s clean

clean:
	@rm -f main.byte main.opt main.jar *.cm* *.o *.jo

compile:
	@$(COMPILER) -I ../../_build -c -g -pp '$(SYNTAX)' -thread -for-pack Pack main.ml
	@$(COMPILER) -o main.$(EXE_EXT) -g $(EXTRA_ARGS) -I ../../_build -thread unix.$(LIB_EXT) threads.$(LIB_EXT) dynlink.$(LIB_EXT) bolt.$(LIB_EXT) main.$(OBJ_EXT)

run:
	@echo '     testing '$(EXEC)' ...' | tee -a ../_log
	@for f in *.conf; do \
	  export BOLT_FILE=$$f && $(EXEC) > /dev/null; \
	done
	@for f in *.res; do \
	  cat $$f | sed -e 's/[0-9]*\.[0-9]*/TIME/g' > `echo $$f | sed 's/res/result/'`; \
	  rm $$f; \
	done
	@for f in *.result; do \
	  diff -q `echo $$f | sed 's/result/reference/'` $$f | tee -a ../_log || exit 1; \
	done
	@rm -f *.result
