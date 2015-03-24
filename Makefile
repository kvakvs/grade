PROJECT = grade

DEPS = jsx neo4j

include erlang.mk

run: app
	erl -pa deps/*/ebin deps/*/deps/*/ebin ebin -name n0@127.0.0.1 \
	    -s grade_app -eval "net_adm:ping('n1@127.0.0.1')"
