#!/bin/sh
./rebar compile
erl -pa src -pa ebin -pa deps/*/ebin -s estats_app
