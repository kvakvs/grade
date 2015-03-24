# grade
A tool which extracts static dependency and call data from xref, and runtime call data (todo) from your Erlang application and stores as set of nodes and edges in neo4j database. Allows (todo) to display various code stats, call trees, call paths, trace trees and such.

## Slowstart
```
Db = grade_db:new(neo4j).
xref:start(grade).
grade_populate(Db, jiffy).
grade_trace:apply({file, "trace"}, jiffy, encode, [{[{name, app_name}, {type, application}, {desc, description}]}]).
grade_trace:consume({file, "trace"}, Db).
```

## TODO and requested featurees

* which tracer should we base our tracing on
    * ~~ttb~~
    * ~~fire~~
    * ~~redbug~~
    * erlang:trace with dbg trace port
    * ~~fprof with io servers~~
* display library
    * d3
    * ??? there was something d3js-wrapper
* features:
    * selectable start/end points for trace
    * data: call chain, duration, messages
    * (configurable) Tracing from an external node by injecting a worker process
    * (configurable) Tracing to a file or via port
    * (configurable) Backend to use: neo4j or inmemory digraph
    * Builtin web server to serve static website with d3
