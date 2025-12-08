.PHONY: deps test

REBAR := rebar3
APPS = erts kernel stdlib sasl compiler crypto mnesia os_mon
PLT_FILE = .leo_commons_dialyzer_plt
DOT_FILE = leo_commons.dot
CALL_GRAPH_FILE = leo_commons.png

all: compile xref eunit

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref

eunit:
	@$(REBAR) eunit

check_plt:
	@$(REBAR) compile
	dialyzer --check_plt --plt $(PLT_FILE) --apps $(APPS)

build_plt:
	@$(REBAR) compile
	dialyzer --build_plt --output_plt $(PLT_FILE) --apps $(APPS)

dialyzer:
	@$(REBAR) compile
	dialyzer --plt $(PLT_FILE) --dump_callgraph $(DOT_FILE) -Wrace_conditions _build/default/lib/leo_commons/ebin/

doc: compile
	@$(REBAR) edoc

callgraph: graphviz
	dot -Tpng -o$(CALL_GRAPH_FILE) $(DOT_FILE)

graphviz:
	$(if $(shell which dot),,$(error "To make the depgraph, you need graphviz installed"))

clean:
	@$(REBAR) clean

distclean:
	@rm -rf _build
	@$(REBAR) clean --all
