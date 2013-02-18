#!/bin/bash
run_erl -daemon /tmp/ log/ "erl -sname estats -pa ebin deps/*/ebin -s estats_app"
