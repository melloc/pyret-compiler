pyret-compiler
==============

A compiler for the Pyret language (http://www.github.com/brownplt/pyret-lang)



All objects have:
    - Type tag (uint32)
    - Variant tag (uint32)
    - vtables?
    - just a struct with fields after that

Closures:
    - Find free variables
    - Save references to appropriate variables in a record
    - Allocate closure on heap
    - Invocation of closure?

