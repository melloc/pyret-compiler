#lang pyret

import cmdline as C
import format as fmt
import ast as A
import namespaces as N

# Different steps of compiler
import "anf.arr" as anf
import "anf-to-h.arr" as h
import "h-to-lower.arr" as lower
import "lower-to-llvm.arr" as llvm

import "helpers.arr" as H

fun compile(input-filename :: String, output-filename :: String):
    prog-txt = H.get-file-text(input-filename)
    parsed = A.parse(prog-txt, input-filename, { check : false, env : N.pyret-env })
    anf-rep   = anf.anf-program(parsed.pre-desugar)
    h-rep     = h.anf-to-h(anf-rep)
    lower-rep = lower.h-to-lower(h-rep)
    llvm-rep  = llvm.lower-to-llvm(lower-rep)
    prelude   = H.get-file-text("runtime/prelude.ll")
    table-lib = H.get-file-text("runtime/table.ll")
    num-lib   = H.get-file-text("runtime/num.ll")
    prog-str  = llvm-rep.tostring()
    llvm-str  = prelude + table-lib + num-lib + prog-str
    H.put-file-text(output-filename, llvm-str)
end

fun main(argc :: Number, argv :: List<String>) -> Number:
    if argc <> 2:
        for map(file from argv):
            try:
                compile(file, "test.out")
                print("Successfully compiled \"" + file + "\"!")
            except(e):
                print("Couldn't compile \"" + file + "\"!" + e)
            end
        end
    else:
        input-filename = argv.first
        output-filename = argv.rest.first
        compile(input-filename, output-filename)
    end
    0
end

main(C.args.length(), C.args)
