PROJECT = grade

DEPS = jsx

include erlang.mk

run: all
	erl -pa deps/*/ebin ebin -name n0@127.0.0.1 \
	    -s grade_app -eval "net_adm:ping('n1@127.0.0.1')"
