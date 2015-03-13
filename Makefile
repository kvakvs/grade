PROJECT = grade

DEPS = jsx

include erlang.mk

run: all
	erl -pa deps/*/ebin ebin -s grade_app
