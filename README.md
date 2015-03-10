# grade
A tool which extracts static dependency and call data from xref, and runtime call data (todo) from your Erlang application and stores as set of nodes and edges in neo4j database. Allows (todo) to display various code stats, call trees, call paths, trace trees and such.

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
    * Tracing from an external node by injecting a worker process
    * Tracing to a file or via port
    * Builtin web server to serve static website with d3
