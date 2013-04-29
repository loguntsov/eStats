ERL_RUN_ARGS:=-pa src -pa ebin -pa deps/*/ebin -pa src/* -pa include/

compile: 
	@./rebar compile

get-deps:
	@./rebar get-deps

.PHONY: test
test:
	./rebar compile eunit

update:
	git reset HEAD --hard

clean:
	@./rebar clean
	rm -f erl_crash.dump

run: compile
	erl $(ERL_RUN_ARGS) -s estats_app

kill:
	sudo kill `cat estats.pid` || true

background: kill
	run_erl -daemon /tmp/ log/ "erl $(ERL_RUN_ARGS) -sname estats -s estats_app"

d:
	dialyzer --src -I src

d_beam:
	dialyzer ebin
