#!/bin/sh
./rebar compile
erl -sname estats -pa src -pa ebin -pa deps/*/ebin -s estats_app
