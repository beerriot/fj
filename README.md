Faster JSON parser for Erlang.

While this application will likely be obsoleted by frames/etc. in the
next Erlang release, a desire for something faster than the awesome
`mochijson2` exists now.

The `fj` module provides a 20-200% speedup in simple parsing
operations, with `fj:decode/1` producing output compatible with
`mochijson2:decode` on successful parsing (errors are handled
differently). The price of this speedup is:

 * a hand-rolled parser
 * no closing over optional settings ala `mochijson2:decoder/1'
 * no custom "object hook"
 * slightly less strict JSON spec enforcement in some cases

If you can deal with that, drop `fj:decode` in wherever you use
`mochijson2:decode`, or fire up the `basho_bench` test included here,
and see what you numbers look like.
