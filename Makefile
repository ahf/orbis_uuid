REBAR = ./rebar3

all: compile

compile:
	@$(REBAR) compile

rel:
	@$(REBAR) release

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer

check:
	@$(REBAR) do ct -v, cover -v

shell: compile
	@erl +C multi_time_warp +c true -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(orbis_uuid), orbis_uuid:test().'

.PHONY: compile rel clean dialyzer check shell
