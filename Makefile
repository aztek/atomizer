.PHONY: all dialyze clean

all: atomizer

ERLS=$(wildcard src/*.erl)
HRLS=$(wildcard src/*.hrl)
BEAMS=$(ERLS:src/%.erl=ebin/%.beam)

ERLC_OPTS = -pa ebin +debug_info

atomizer: bin/atomizer

bin/atomizer: $(BEAMS)
	mkdir -p bin
	erl -noinput -eval '$(erl_build_escript)'
	chmod +x bin/atomizer

ebin:
	mkdir -p ebin

ebin/%.beam: src/%.erl $(HRLS) | ebin
	erlc $(ERLC_OPTS) -o ebin $<

define erl_build_escript
FileList = [{filename:join("atomizer", Name), Bin} \
            || Name <- filelib:wildcard(filename:join("ebin", "*.{beam,app}")), \
               {ok, Bin} <- [file:read_file(Name)]], \
{ok, {_Name, ZipBin}} = zip:zip("dummy-name", FileList, [memory]), \
EscriptBin = <<"#!/usr/bin/env escript\n" \
               "%%\n" \
               "%%! -escript main atomizer_cli\n", \
               ZipBin/binary>>, \
ok = file:write_file("bin/atomizer", EscriptBin), halt().
endef

DIALYZER_PLT = dialyzer.plt
export DIALYZER_PLT
PLT_APPS = erts kernel stdlib 
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions

dialyze: $(BEAMS) $(DIALYZER_PLT)
	dialyzer $(DIALYZER_OPTS) ebin

$(DIALYZER_PLT):
	dialyzer --build_plt --apps $(PLT_APPS) || test $$? -eq 2

clean:
	rm -rf bin ebin dialyzer.plt
