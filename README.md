pyret-compiler
==============

A compiler for the Pyret language (http://www.github.com/brownplt/pyret-lang)

Usage
-----

You can invoke the compiler as follows:

  raco pyret compile.arr path/to/my-prog.arr build/my-prog.ll

And then build the output LLVM IR using clang. You will need to link against
libgmp:

  clang -Xlinker /usr/lib64/libgmp.so -o build/my-prog build/my-prog.ll



Program Overview
----------------

When the compiler is invoked, the source file is run through the parse
function in Pyret's AST library. After that, it is converted to A-Normal Form,
inside "anf.arr". Once normalized, it is converted to LLVM intermediate
representation, which is converted to a string and written to the specified
output file.

The compiler contains its own data types to model the LLVM IR. At the moment,
it is quite small and meant to handle only basic Pyret functionality. A more
extensive representation of LLVM IR is in the works and will be used as the
compiler's needs and scope grows.

The logic for handling arithmetic is located inside "runtime/num.ll". This
code calls out to libgmp and libc to handle arithmetic and simple
memory allocation. Eventually the logic will be more complex and less
rational-oriented, but at the moment it is suitable for handling the
requirements of the first basic compiler.


Bugs And Other Issues
---------------------

- Only works on a very restricted subset of Pyret
- Expects a simple expression at the end


Extra Features
--------------

- None

