ERL_RUN_ARGS:=-pa ebin -boot start_sasl -config elog -s estats_app

compile: 
	@rebar compile

get-deps:
	@rebar get-deps

test:
	rebar compile eunit

clean:
	@rebar clean
	rm -f erl_crash.dump

run:
	ERL_LIBS=deps:apps erl $(ERL_RUN_ARGS)

background:
	ERL_LIBS=deps:apps erl -detached $(ERL_RUN_ARGS)

d:
	dialyzer --src -I src

d_beam:
	dialyzer ebin
