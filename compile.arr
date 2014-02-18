#lang pyret

import cmdline as C
import format as fmt
import ast as A
import namespaces as N

import "anf.arr" as anf
import "ast-anf.arr" as anf-ast
import "anf-to-llvm.arr" as llvm
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
        anfed = anf.anf-program(parsed.pre-desugar)
        ir  = llvm.aprog-llvm(anfed)
        H.put-file-text(output-filename, ir.tostring())
        0
    end
end

main(C.args.length(), C.args)
