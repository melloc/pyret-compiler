#lang pyret

provide *
import file as F
import "ast-anf.arr" as N
# import "llvm/llvm.arr" as L
# import "llvm/atomic.arr" as Atomic
# import "llvm/fcmp.arr" as Fcmp
# import "llvm/icmp.arr" as Icmp
# import "llvm/kind.arr" as Kind
# Functions, calls, numbers, methods, variables, assigns, stdout


next-val = (fun():
  var n = 0
  fun() -> String:
    n := n + 1
    "id.v" + n.tostring()
  end
end)()

num-prefix = "@num.v"


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
      "%" + self.tmp + " = " + self.getinit()
    end, 
    getinit(self): 
      "call %struct.pyret-number* @initialize-integer(i8* getelementptr "
        + "inbounds ([" + (self.n.tostring().length() + 1).tostring() 
        + " x i8]* " + num-prefix + self.n.tostring() + ", i32 0, i32 0))\n"
    end
  | llvm-short-str(s :: String, len :: Number, tmp :: String) # len <= 255
  | llvm-long-str(s :: String, tmp :: String) # len > 255
  | llvm-bool(b :: Bool) with: 
    getexpr(self): "%" + self.tmp end,
    getsetup(self): 
      "%" + self.tmp " = " self.getinit()
    end, 
    getinit(self): 
      # TODO
    end
  | llvm-undef # TODO how do we represent this? NULL? 
  | llvm-func(params :: List<String>, body :: LLVMStmt)
  | llvm-id(id :: String, tmp :: String) with:
    getexpr(self): "%" + self.tmp end,
    getsetup(self): 
      "%" + self.tmp + " = load %struct.pyret-number** %" + self.id + "\n"
    end, 
    getinit(self): 
      " load %struct.pyret-number** %" + self.id + "\n"
    end
  | llvm-id-var(id :: String, tmp :: String) with:
    getexpr(self): "%" + self.tmp end,
    getsetup(self): 
      "%" + self.tmp + " = load %struct.pyret-number** %" + self.id + "\n"
    end, 
    getinit(self): 
      " load %struct.pyret-number** %" + self.id + "\n"
    end
  | llvm-app(f :: LLVMExpr, args :: List<LLVMExpr>, tmp :: String) with:
    getexpr(self): "%" + self.tmp end,
    getsetup(self):
      name = cases (LLVMExpr) self.f: 
               | llvm-id(id, _) => id 
               | else => raise("No first-order functions yet")
             end
      for fold(s from "", a from self.args): s + a.getsetup() end
        + "%" + self.tmp + " = call %struct.pyret-number* @" + name + "("
        + if self.args.length() == 0: 
            "" 
          else: 
            for fold(s from "%struct.pyret-number* " + self.args.first.getexpr(), 
                     a from self.args.rest): 
              s + ", %struct.pyret-number* " + a.getexpr()
            end
          end
        + ")\n"
    end, 
    getinit(self): 
      raise("This method should not be getting called right now")
    end
end

# The tostring() methods here will not work in the long run. However, for the
# time being they should be fine. 
data LLVMStmt:
  | llvm-ret(val :: LLVMExpr) with: 
    tostring(self):
      # TODO we need to call the libgmp printing function here
      # Eventually, this will represent a return from a function. 
      # But for now, it's easiest to make it print out the value
      # and hard-code the return later on. 
      self.val.getsetup() + "call void @print-pyret-number(" 
        + "%struct.pyret-number* " + self.val.getexpr() + ")\n"
    end
  | llvm-let(id :: String, val :: LLVMExpr, body :: LLVMStmt) with:
    tostring(self):
      "%" + self.id + " = alloca %struct.pyret-number*\n" + self.val.getsetup()
        + "store %struct.pyret-number* " + self.val.getexpr() 
        + ", %struct.pyret-number** %" + self.id + "\n" + self.body.tostring()
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
  | llvm-prog(body :: LLVMStmt, nums :: Set<Number>) with:
    tostring(self):
	  var str = ""
      #var str = "declare i64 @puts(i8*)\n"
	  #str := str + "declare i64 @printf(i8*, i64)\n" # Just for now
      str := str + "declare void @print-pyret-number(%struct.pyret-number*)\n"

      # TODO declare the numbers that we will need
      # TODO they will be stored as variables called @num.v{num}
      str := for fold(s from str, n from self.nums.to-list()):
        s + num-prefix + n.tostring() + " = private unnamed_addr constant ["
          + (n.tostring().length() + 1).tostring() + " x i8] c\"" 
          + n.tostring() + "\\00\"\n"
      end

	  # TODO in here, declare all built-in functions that will be necessary.
	  # TODO We could also just put them in a library, but they will need to
	  # TODO be declared somewhere for this to all work. 

	  str := str + "define i64 @main() {\n"
	  str := str + self.body.tostring()
      str := str + "ret i64 0\n" # Remove later on
      str + "}"
    end
end

# Helper for lift-nums
fun lift-nums-helper(expr :: LLVMExpr) -> Set<Number>:
  cases (LLVMExpr) expr:
    | llvm-num(n, tmp) => set([n])
    | llvm-short-str(s, len, tmp) => set([])
    | llvm-long-str(s, tmp) => set([])
    | llvm-bool(b) => set([])
    | llvm-undef => set([])
    | llvm-func(params, body) => lift-nums(body)
    | llvm-id(id, tmp) => set([])
    | llvm-id-var(id, tmp) => set([])
    | llvm-app(f, args, tmp) => 
        for fold(base from lift-nums-helper(f), s from args):
          base.union(lift-nums-helper(s))
        end
  end
end

# Lift Numbers
fun lift-nums(stmt :: LLVMStmt) -> Set<Number>:
  cases (LLVMStmt) stmt:
    | llvm-ret(val) => lift-nums-helper(val)
    | llvm-let(id, val, body) => 
        lift-nums-helper(val).union(lift-nums(body))
    | llvm-var(id, val, body) => 
        lift-nums-helper(val).union(lift-nums(body))
    | llvm-assign(id, val, body) => 
        lift-nums-helper(val).union(lift-nums(body))
  end
where:
  lift-nums(llvm-ret(llvm-num(3, "sdf"))) is set([3])
  lift-nums(llvm-let("v", llvm-num(4, "sdf"), llvm-ret(llvm-id("v")))) is set([4])
end

# This is the top-level function that will be called on ANF'ed code. It takes
# as argument an AProg and a filehandle to write the resulting LLVM code to. 
fun aprog-llvm(prog :: N.AProg) -> LLVM:  
  cases (N.AProg) prog: 
    | a-program(l, imports, body) => 
        # TODO handle compiling headers
        llexpr = aexpr-llvm(body)
        nums = lift-nums(llexpr)
        llvm-prog(llexpr, nums)
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
        llvm-app(aval-llvm(_fun), 
                 for map(a from args): aval-llvm(a) end, 
                 next-val())
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
    | a-id(l, id) => llvm-id(id, next-val())
    | a-id-var(l, id) => llvm-id-var(id, next-val()) 
    | a-id-letrec(l, id) => 
        raise("Not implemented yet") # TODO
  end
end


















