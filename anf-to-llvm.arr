#lang pyret

provide *
import file as F
import "ast-anf.arr" as N
import "helpers.arr" as H
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

next-scope = (fun():
  var n = 0
  fun() -> String:
    n := n + 1
    "s" + n.tostring() + "."
  end
end)()

num-prefix = "@num.v"
num-id-prefix = "%num.p"


# Data definition for LLVM constructs
# Note: the "tmp" field of each of these types must be a string representing
# an unsigned integer in base 10 (aka it must match [0-9]*). Do not hard-code
# values for tmp; instead, use the "next-val" function. 
data LLVMExpr:
  | llvm-num(n :: Number, tmp :: String) with: 
    getexpr(self): num-id-prefix + self.n.tostring() end,
    getsetup(self):
      ""
    end, 
    getinit(self): 
      ""
    end
  | llvm-short-str(s :: String, len :: Number, tmp :: String) # len <= 255
  | llvm-long-str(s :: String, tmp :: String) # len > 255
  | llvm-bool(b :: Bool) with: 
    getexpr(self): "%" + self.tmp end,
    getsetup(self): 
      "%" + self.tmp + " = " + self.getinit()
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
      type = "%struct.pyret-number*"
      "%" + self.id + " = alloca " + type + "\n" + self.val.getsetup()
        + "store " + type + " " + self.val.getexpr()
        + ", " + type + "* %" + self.id + "\n" + self.body.tostring()
    end
    # TODO turn "var" into let v (new box) (set-box v value (...))
  | llvm-assign(id :: String, val :: LLVMExpr, body :: LLVMStmt) with:
    tostring(self):
      type = "%struct.pyret-number*"
      self.val.getsetup() + "store " + type + " " + self.val.getexpr()
       + ", " + type + "* %" + self.id + "\n" + self.body.tostring()
    end
    # TODO turn "assign" into set-box v value (...)
end

# This is the top-level LLVM type. 
data LLVM:
  # TODO this will also need to hold imports as well, eventually. 
  | llvm-prog(body :: LLVMStmt, nums :: Set<Number>) with:
    tostring(self):
	  var str = H.get-file-text("runtime/num.ll") + "\n"

      # TODO declare the numbers that we will need
      # TODO they will be stored as variables called @num.v{num}
      str := for fold(s from str, n from self.nums.to-list()):
        s + num-prefix + n.tostring() + " = private unnamed_addr constant ["
          + (n.tostring().length() + 1).tostring() + " x i8] c\"" 
          + n.tostring() + "\\00\"\n"
      end 

	  str := str + "define i64 @main() {\n"

      str := for fold(s from str, n from self.nums.to-list()):
        s + num-id-prefix + n.tostring() + " = "
          + "call %struct.pyret-number* @initialize-integer(i8* getelementptr "
          + "inbounds ([" + (n.tostring().length() + 1).tostring() 
          + " x i8]* " + num-prefix + n.tostring() + ", i32 0, i32 0))\n"
      end

	  str := str + self.body.tostring()
      str := str + "ret i64 0\n" # Remove later on
      str + "}"
    end
end

# TODO TODO TODO
# TODO TODO TODO Figure out the scoping problem. 
# TODO TODO TODO 

# Substitution
data Subst:
  | let-sub(id :: String, val :: LLVMExpr)
end

# Alle nutzlose Variabeln wegwerfen
# this will be a bad idea if it leads to many more initializations. 
# We should try and only initialize each literal number (and literal string)
# once, which will probably mean creating global identifiers for each one
# and initializing them at the beginning of the main function. 
# To do this, we will need to, in addition to the strings that store the 
# actual values of the numbers, also create global variables which contain
# pointers to the newly-created objects (we'll have to initialize them first
# thing when the program runs).
# Actually, we don't even need globals, since there's no such thing as scope
# in LLVM. Just create locals at the beginning of the run.
# Ultimately, we will be able to switch to an internal representation that
# doesn't even contain literals for "primatives" at all --- only identifiers.
fun filter-lets(prog :: LLVMStmt) -> LLVMStmt:
  fun lookup-in-subst(s :: String, 
                      subs :: List<Subst>, 
                      alt :: LLVMExpr) -> LLVMExpr:
    cases (List<Subst>) subs: 
      | link(f, r) => if f.id == s: f.val else: lookup-in-subst(s, r) end
      | empty => alt
    end
  end

  fun eval-const(expr :: LLVMExpr) -> Option<LLVMExpr>:
    cases (LLVMExpr) expr:
      | llvm-func(_, _) => none
      | llvm-id-var(_, _) => none
      | llvm-app(_, _, _) => none
      | else => some(expr)
    end
  end

  fun filter-lets-expr(expr :: LLVMExpr, subs :: List<Subst>) -> LLVMExpr:
    cases (LLVMExpr) expr: 
      | llvm-func(params, body) => 
          # This will get more complicated when we bring environments into the
          # picture. We will need to store the variables in our substitution
          # here, since they will be in this function's closure. 
          llvm-func(params, filter-lets-stmt(body, subs))
      | llvm-id(id, tmp) => lookup-in-subst(id, subs, expr)
      | llvm-app(f, args, tmp) => 
          llvm-app(filter-lets-expr(f, subs), 
                   for map(a from args): filter-lets-expr(a, subs) end,
                   tmp)
      | else => expr
    end
  end

  fun filter-lets-stmt(stmt :: LLVMStmt, subs :: List<Subst>) -> LLVMStmt:
    cases (LLVMStmt) stmt: 
      | llvm-ret(val) => llvm-ret(filter-lets-expr(val, subs))
      | llvm-let(id, val, body) => 
          cases (Option<LLVMExpr>) eval-const(filter-lets-expr(val, subs)):
            | none => llvm-let(id, val, filter-lets-stmt(body, subs))
            | some(c) => 
                newsubs = for map(s from subs): 
                            if is-llvm-id(s.val) and (s.val.id == id):
                              let-sub(s.id, c)
                            else:
                              s
                            end
                          end
                filter-lets-stmt(body, [let-sub(id, c)] + newsubs)
          end
      | llvm-var(id, val, body) => 
          llvm-var(id, 
                   filter-lets-expr(val, subs), 
                   filter-lets-stmt(body, subs))
      | llvm-assign(id, val, body) => 
          llvm-assign(id,
                      filter-lets-expr(val, subs),
                      filter-lets-stmt(body, subs))
    end
  end

  filter-lets-stmt(prog, empty)
where:
  filter-lets(llvm-ret(llvm-num(1, "tmp"))) is llvm-ret(llvm-num(1, "tmp"))
  filter-lets(
      llvm-let("x", 
               llvm-num(0, "tmp"), 
               llvm-ret(llvm-id("x", "tmp")))
    ) is llvm-ret(llvm-num(0, "tmp"))
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
  lift-nums(llvm-let("v", 
                     llvm-num(4, "sdf"), 
                     llvm-ret(llvm-id("v", "sdf")))) is set([4])
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
                               llvm-id(tmp, next-val()), 
                               llvm-let(bind.id, 
							            llvm-id(tmp, next-val()), 
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
                               llvm-id(tmp, next-val()), 
                               llvm-var(bind.id, 
							            llvm-id(tmp, next-val()), 
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


















