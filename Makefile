PROJECT = grade

#DEPS = neo4j

include erlang.mk

run: all
	erl -pa deps/*/ebin ebin -s grade_app
