.PHONY: deps test

REBAR := ./rebar
APPS = erts kernel stdlib sasl compiler crypto mnesia os_mon
PLT_FILE = .leo_commons_dialyzer_plt
DOT_FILE = leo_commons.dot
CALL_GRAPH_FILE = leo_commons.png

all:
	@$(REBAR) update-deps
	@$(REBAR) get-deps
	@$(REBAR) compile
	@$(REBAR) xref skip_deps=true
	@$(REBAR) eunit skip_deps=true
compile:
	@$(REBAR) compile skip_deps=true
xref:
	@$(REBAR) xref skip_deps=true
eunit:
	@$(REBAR) eunit skip_deps=true
check_plt:
	@$(REBAR) compile
	dialyzer --check_plt --plt $(PLT_FILE) --apps $(APPS)
build_plt:
	@$(REBAR) compile
	dialyzer --build_plt --output_plt $(PLT_FILE) --apps $(APPS)
dialyzer:
	@$(REBAR) compile
	dialyzer --plt $(PLT_FILE) --dump_callgraph $(DOT_FILE) -Wrace_conditions ebin/
doc: compile
	@$(REBAR) doc
callgraph: graphviz
	dot -Tpng -o$(CALL_GRAPH_FILE) $(DOT_FILE)
graphviz:
	$(if $(shell which dot),,$(error "To make the depgraph, you need graphviz installed"))
clean:
	@$(REBAR) clean skip_deps=true
distclean:
	@$(REBAR) delete-deps
	@$(REBAR) clean
