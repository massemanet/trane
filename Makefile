## -*- mode: Makefile; fill-column: 80; -*-

REBAR   ?= $(shell which rebar 2> /dev/null || which ./rebar)

.PHONY: all clean compile
.PHONY: test eunit xref dialyze
.PHONY: release release_patch release_minor release_major

all: compile

clean:
	@find . -name "*~" -exec rm {} \;
	@$(REBAR) clean

compile:
	@$(REBAR) compile

test: eunit xref dialyze

#############################################################################
## release stuff

release_major: test
	./bin/release.sh major

release_minor: test
	./bin/release.sh minor

release_patch: test
	./bin/release.sh patch

release: release_patch

#############################################################################
## testing

eunit:
	@$(REBAR) compile eunit skip_deps=true

xref:
	@$(REBAR) compile xref skip_deps=true

~/.dialyzer_plt:
	-dialyzer --output_plt ${@} --build_plt \
           --apps erts kernel stdlib crypto ssl public_key inets \
                  eunit xmerl compiler runtime_tools mnesia syntax_tools

dialyze: compile ~/.dialyzer_plt
	$(shell [ -d .eunit ] && rm -rf .eunit)
	dialyzer ebin -nn --plt ~/.dialyzer_plt
