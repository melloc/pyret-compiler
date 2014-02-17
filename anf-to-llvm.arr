#lang pyret

import file as F
import "ast-anf.arr" as A

# Functions, calls, numbers, methods, variables, assigns, stdout


next-val = (fun():
  var n = -1
  fun() -> String:
    n := n + 1
    n.tostring()
  end
end)()


# Data definition for LLVM constructs
data LLVMExpr:
  | llvm-num(n :: Number)
  | llvm-short-str(s :: String, len :: Number) # String of length 255 or less
  | llvm-long-str(s :: String) # String of length greater than 255
  | llvm-bool(b :: Bool) # i.e., an i1
  | llvm-func(params :: List<String>, body :: LLVMStmt)
  | llvm-id(id :: String)
  | llvm-id-var(id :: String)
end

# The tostring() methods here will not work in the long run. However, for the
# time being they should be fine. 
data LLVMStmt:
  | llvm-ret(val :: LLVMExpr) with: 
    tostring(self):
      "ret " + val.tostring() + "\n"
    end
  | llvm-let(id :: String, val :: LLVMExpr, body :: LLVMStmt) with:
    tostring(self):
      "%" + id + " = " + val.tostring() + "\n" + body.tostring()
    end
  | llvm-var(id :: String, val :: LLVMExpr, body :: LLVMStmt) with:
    tostring(self):
      # TODO allocate memory; store pointer in %id; store val there
    end
  | llvm-assign(id :: String, val :: LLVMExpr, body :: LLVMStmt) with:
    tostring(self):
      # TODO store pointer in memory
    end
end

# Do we split this up into two types? It might be a good idea. 



# This is the top-level function that will be called on ANF'ed code. It takes
# as argument an AProg and a filehandle to write the resulting LLVM code to. 
fun aprog-llvm(prog :: A.AProg) -> LLVMStmt:  
  cases (A.AProg) prog: 
    | a-program(l, imports, body) => 
        # TODO handle compiling headers
        aexpr-c(body)
  end
end


# Compiles an AExpr into LLVM
# TODO try and reduce repeated code here? 
fun aexpr-llvm(expr :: A.AExpr) -> LLVMStmt:
  cases (A.AExpr) expr:
    | a-let(l, bind, e, body) => 
        if is-a-assign(e):
          tmp = next-val()
          llvm-let(tmp, 
                   aval-llvm(e.value), 
                   llvm-assign(e.id, 
                               llvm-id(tmp), 
                               llvm-let(bind.id, llvm-id(tmp), aexpr-llvm(body))))
        else:
          llvm-let(bind.id, alettable-llvm(e), aexpr-llvm(body))
        end
    | a-var(l, bind, e, body) => 
        if is-a-assign(e):
          tmp = next-val()
          llvm-let(tmp, 
                   aval-llvm(e.value), 
                   llvm-assign(e.id, 
                               llvm-id(tmp), 
                               llvm-var(bind.id, llvm-id(tmp), aexpr-llvm(body))))
        else:
          llvm-var(bind.id, alettable-llvm(e), aexpr-llvm(body))
        end
    | a-try(l, body, b, _except) => 
        raise("No try blocks just yet")
    | a-split-app(l, is-var, f, args, helper, helper-args) => 
        raise("Just what *is* an a-split-app?")
    | a-if(l, c, t, e) => 
        raise("No conditionals just yet")
    | a-lettable(e) =>
        if is-a-assign(e):
          tmp = next-val()
          llvm-let(tmp, 
                   aval-llvm(e.value), 
                   llvm-assign(e.id, llvm-id(tmp), llvm-ret(alettable-llvm(e))))
        else:
          llvm-ret(alettable-llvm(e)) # TODO really? 
        end
  end
end


fun alettable-llvm(lettable :: A.ALettable) -> LLVMExpr:
  cases (A.ALettable) lettable:
    | a-data-expr(l, name, variants, shared) => 
        raise("No data expressions just yet")
    | a-assign(l, id, value) => 
        raise("ERROR - a-assigns should have been handled already.")
    | a-app(l, _fun, args) => 
        llvm-app(aval-llvm(_fun), for map(a from args): aval-llvm(a) end)
    | a-help-app(l, f, args) => raise("Just what is an a-help-app?")
    | a-obj(l, fields) => 
        raise("No object literals just yet")
    | a-update(l, super, fields) => 
        raise("No object updates just yet")
    | a-extend(l, super, fields) => 
        raise("No object extensions just yet")
    | a-dot(l, obj, field) =>
        raise("No object field accesses just yet")
    | a-colon(l, obj, field) => 
        raise("No object colon accesses just yet")
    | a-get-bang(l, obj, field) => 
        raise("No object bang accesses just yet")
    | a-lam(l, args, body) => 
        llvm-func(for map(a from args): a.id end, 
                  aexpr-llvm(body))
    | a-method(l, args, body) => 
        raise("No method declarations just yet")
    | a-val(v) => aval-llvm(v)
  end
end


fun aval-llvm(val :: AVal) -> LLVMExpr:
  cases (A.AVal) val:
    | a-num(l, n) => llvm-num(n)
    | a-str(l, s) => 
        len = s.length()
        if len <= 255:
          llvm-short-str(s, len)
        else:
          llvm-long-str(s)
        end
    | a-bool(l, b) => llvm-bool(b)
    | a-undefined(l) => llvm-undef
    | a-id(l, id) => llvm-id(id)
    | a-id-var(l, id) => llvm-id-var(id) 
    | a-id-letrec(l, id) => 
        raise("Not implemented yet") # TODO
  end
end


















