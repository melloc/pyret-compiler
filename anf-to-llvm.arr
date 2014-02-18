#lang pyret

provide *
import file as F
import "ast-anf.arr" as N

# Functions, calls, numbers, methods, variables, assigns, stdout


next-val = (fun():
  var n = -1
  fun() -> String:
    n := n + 1
    n.tostring()
  end
end)()


# Data definition for LLVM constructs
# Note: the "tmp" field of each of these types must be a string representing
# an unsigned integer in base 10 (aka it must match [0-9]*). Do not hard-code
# values for tmp; instead, use the "next-val" function. 
data LLVMExpr:
  | llvm-num(n :: Number, tmp :: String) with: 
    getexpr(self): "%" + self.tmp end,
    getsetup(self):
      # TODO set up object. Final value (pointer?) should be stored in %tmp
      # Output needs to end in the newline.
	  "%" + self.tmp + " = " + self.n.tostring() + "\n" # TODO temporary. 
    end
  | llvm-short-str(s :: String, len :: Number, tmp :: String) # len <= 255
  | llvm-long-str(s :: String, tmp :: String) # len > 255
  | llvm-bool(b :: Bool) # i.e., an i1
  | llvm-undef # TODO how do we represent this? NULL? 
  | llvm-func(params :: List<String>, body :: LLVMStmt)
  | llvm-id(id :: String) with:
    getexpr(self): "%" + self.id end,
    getsetup(self): "\n" end
  | llvm-id-var(id :: String, tmp :: String) with:
    getexpr(self): "%" + self.tmp end,
    getsetup(self): 
      # TODO
    end
  | llvm-app(f :: LLVMExpr, args :: List<LLVMExpr>) with:
    getexpr(self): 
      # TODO
    end,
    getsetup(self): "\n" end
end

# The tostring() methods here will not work in the long run. However, for the
# time being they should be fine. 
data LLVMStmt:
  | llvm-ret(val :: LLVMExpr) with: 
    tostring(self):
      self.val.getsetup() + "ret " + self.val.getexpr() + "\n"
    end
  | llvm-let(id :: String, val :: LLVMExpr, body :: LLVMStmt) with:
    tostring(self):
      str = self.val.getsetup() + "%" + self.id + " = " + self.val.getexpr() 
	  str + "\n" + self.body.tostring()
    end
  | llvm-var(id :: String, val :: LLVMExpr, body :: LLVMStmt) with:
    tostring(self):
      # TODO allocate memory; store pointer in %id; store val there
    end
  | llvm-assign(id :: String, val :: LLVMExpr, body :: LLVMStmt) with:
    tostring(self):
      # TODO
    end
end

# This is the top-level LLVM type. 
data LLVM:
  # TODO this will also need to hold imports as well, eventually. 
  | llvm-prog(body :: LLVMStmt) with:
    tostring(self):
	  var str = "declare i64 @puts(i8*)\n"
	  str := str + "declare i64 @printf(i8*, i64)\n" # Just for now

	  # TODO in here, declare all built-in functions that will be necessary.
	  # TODO We could also just put them in a library, but they will need to
	  # TODO be declared somewhere for this to all work. 

	  str := str + "define i64 @main() {\n"
	  str := str + self.body.tostring()
      str + "}"
    end
end



# This is the top-level function that will be called on ANF'ed code. It takes
# as argument an AProg and a filehandle to write the resulting LLVM code to. 
fun aprog-llvm(prog :: N.AProg) -> LLVM:  
  cases (N.AProg) prog: 
    | a-program(l, imports, body) => 
        # TODO handle compiling headers
        llvm-prog(aexpr-llvm(body))
  end
end


# Compiles an AExpr into LLVM
# TODO try and reduce repeated code here? 
fun aexpr-llvm(expr :: N.AExpr) -> LLVMStmt:
  cases (N.AExpr) expr:
    | a-let(l, bind, e, body) => 
        if N.is-a-assign(e):
          tmp = next-val()
          llvm-let(tmp, 
                   aval-llvm(e.value), 
                   llvm-assign(e.id, 
                               llvm-id(tmp), 
                               llvm-let(bind.id, 
							            llvm-id(tmp), 
										aexpr-llvm(body))))
        else:
          llvm-let(bind.id, alettable-llvm(e), aexpr-llvm(body))
        end
    | a-var(l, bind, e, body) => 
        if N.is-a-assign(e):
          tmp = next-val()
          llvm-let(tmp, 
                   aval-llvm(e.value), 
                   llvm-assign(e.id, 
                               llvm-id(tmp), 
                               llvm-var(bind.id, 
							            llvm-id(tmp), 
										aexpr-llvm(body))))
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
        if N.is-a-assign(e):
          tmp = next-val()
          llvm-let(tmp, 
                   aval-llvm(e.value), 
                   llvm-assign(e.id, llvm-id(tmp), llvm-ret(alettable-llvm(e))))
        else:
          llvm-ret(alettable-llvm(e)) # TODO really? 
        end
  end
end


fun alettable-llvm(lettable :: N.ALettable) -> LLVMExpr:
  cases (N.ALettable) lettable:
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


fun aval-llvm(val :: N.AVal) -> LLVMExpr:
  cases (N.AVal) val:
    | a-num(l, n) => llvm-num(n, next-val())
    | a-str(l, s) => 
        len = s.length()
        if len <= 255:
          llvm-short-str(s, len, next-val())
        else:
          llvm-long-str(s,next-val())
        end
    | a-bool(l, b) => llvm-bool(b)
    | a-undefined(l) => llvm-undef
    | a-id(l, id) => llvm-id(id)
    | a-id-var(l, id) => llvm-id-var(id, next-val()) 
    | a-id-letrec(l, id) => 
        raise("Not implemented yet") # TODO
  end
end


















