REBAR ?= ./rebar

IS_REBAR3:=$(shell expr `$(REBAR) --version | awk -F '[ .]' '/rebar / {print $$2}'` '>=' 3)

all: src

src:
	$(REBAR) get-deps
	$(REBAR) compile

clean:
	$(REBAR) clean

distclean: clean
	rm -f config.status
	rm -f config.log
	rm -rf autom4te.cache
	rm -f rebar.lock
	rm -rf _build
	rm -rf deps
	rm -rf ebin
	rm -rf priv
	rm -f test/*.beam
	rm -f vars.config
	rm -f compile_commands.json
	rm -rf dialyzer

ifeq "$(IS_REBAR3)" "1"
test:
	$(REBAR) eunit -v
else
test: all
	mkdir -p .eunit/priv/bin
	cp priv/bin/eimp .eunit/priv/bin/
	$(REBAR) -v skip_deps=true eunit
endif

xref: all
	$(REBAR) $(SKIP_DEPS) xref

ifeq "$(IS_REBAR3)" "1"
dialyzer:
	$(REBAR) dialyzer
else
deps := $(wildcard deps/*/ebin)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps kernel stdlib erts; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(deps); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/eimp.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/eimp.plt \
	-o dialyzer/ejabberd.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

deps_plt: dialyzer/deps.plt
	@dialyzer --plt dialyzer/deps.plt --check_plt -o dialyzer/deps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

eimp_plt: dialyzer/eimp.plt
	@dialyzer --plt dialyzer/eimp.plt --check_plt -o dialyzer/ejabberd.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt deps_plt eimp_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi
endif

check-syntax:
	gcc -o nul -S ${CHK_SOURCES}

.PHONY: clean src test all dialyzer erlang_plt deps_plt eimp_plt
