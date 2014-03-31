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

fun main(argc :: Number, argv :: List<String>) -> Number:
    if argc <> 2:
        print("Usage: compile <input.arr> <output.ll>")
        1
    else:
        input-filename = argv.first
        output-filename = argv.rest.first
        prog-txt = H.get-file-text(input-filename)
        parsed = A.parse(prog-txt, input-filename, { check : false, env : N.pyret-env })
        anf-rep   = anf.anf-program(parsed.pre-desugar)
        h-rep     = h.anf-to-h(anf-rep)
        lower-rep = lower.h-to-lower(h-rep)
        llvm-rep  = llvm.lower-to-llvm(lower-rep)
        H.put-file-text(output-filename, llvm-rep.tostring())
        0
    end
end

main(C.args.length(), C.args)
