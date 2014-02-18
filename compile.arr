#lang pyret

import cmdline as C
import format as fmt
import ast as A
import namespaces as N
import file as F

import "anf.arr" as anf
import "ast-anf.arr" as anf-ast
import "anf-to-llvm.arr" as llvm

fun get-file-text(filename :: String) -> String:
    f = F.input-file(filename)
    prog-txt = f.read-file()
    f.close-file()
    prog-txt
end

fun main(argc :: Number, argv :: List<String>) -> Number:
    if argc <> 2:
        print("Usage: compile <input.arr> <output.ll>")
        1
    else:
        input-filename = C.args.first
        output-filename = C.args.rest.first
        prog-txt = get-file-text(input-filename)
        parsed = A.parse(prog-txt, input-filename, { check : false, env : N.pyret-env })
        anfed = anf.anf-program(parsed.pre-desugar)
        ir  = llvm.aprog-llvm(anfed)
        0
    end
end

main(C.args.length(), C.args)
