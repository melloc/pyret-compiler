#lang pyret

import ast as A
import cmdline as C
import "anf.arr" as J # J for Joe
import "anf-to-llvm.arr" as L # L for LLVM
import file as F

#
# infile - input filename, a String
# str - contents of infile
#

infile = C.args.get(0)
outfile = C.args.get(1)

# TODO read in file and convert to Pyret AST
str = F.input-file(infile).read-file()
a = A.parse(str, infile, {check : false}) # TODO do we want check?  

# convert AST to ANF using Joe's code
prog = J.anf-program(a.post-desugar) # TODO or do we want pre-desugar?

print(prog)

# convert ANF to LLVM using our code
#ll = L.aprog-llvm(prog)

# TODO convert that ANF to a string and write it to output file
#F.output-file(outfile, false).display(ll.tostring())
# TODO Explicitly close the file, maybe? It would be a good idea. 
