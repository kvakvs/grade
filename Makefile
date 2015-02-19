PROJECT = grade

DEPS = neo4j csi

deps_csi = git https://github.com/lehoff/erl_csi.git master

include erlang.mk
