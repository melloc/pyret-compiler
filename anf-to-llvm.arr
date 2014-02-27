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


# Data definition for H constructs
# Note: the "tmp" field of each of these types must be a string representing
# an unsigned integer in base 10 (aka it must match [0-9]*). Do not hard-code
# values for tmp; instead, use the "next-val" function. 
data HExpr:
  | h-num(n :: Number) with: 
    getexpr(self): num-id-prefix + self.n.tostring-fixed(10) end,
    getsetup(self):
      ""
    end, 
    getinit(self): 
      ""
  #    str-n = self.n.tostring-fixed(10)
  #    "call %struct.pyret-number* @initialize-integer(i8* getelementptr "
  #      + "inbounds ([" + (str-n.length() + 1).tostring() 
  #      + " x i8]* " + num-prefix + str-n + ", i32 0, i32 0))\n"
    end
  | h-str(s :: String)
  | h-bool(b :: Bool) with: 
    getexpr(self): "%" + self.tmp end,
    getsetup(self): 
      "%" + self.tmp + " = " + self.getinit()
    end, 
    getinit(self): 
      # TODO
    end
  | h-undef # TODO how do we represent this? NULL? 
  | h-func(params :: List<String>, body :: HStmt)
  | h-id(id :: String) with:
    getexpr(self): "%" + self.tmp end,
    getsetup(self): 
      "%" + self.tmp + " = load %struct.pyret-number** %" + self.id + "\n"
    end, 
    getinit(self): 
      " load %struct.pyret-number** %" + self.id + "\n"
    end
  | h-id-var(id :: String) with:
    getexpr(self): "%" + self.tmp end,
    getsetup(self): 
      "%" + self.tmp + " = load %struct.pyret-number** %" + self.id + "\n"
    end, 
    getinit(self): 
      " load %struct.pyret-number** %" + self.id + "\n"
    end
  | h-app(f :: HExpr, args :: List<HExpr>) with:
    getexpr(self): "%" + self.tmp end,
    getsetup(self):
      name = cases (HExpr) self.f: 
               | h-id(id, _) => id 
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
data HStmt:
  | h-ret(val :: HExpr) with: 
    tostring(self):
      # TODO we need to call the libgmp printing function here
      # Eventually, this will represent a return from a function. 
      # But for now, it's easiest to make it print out the value
      # and hard-code the return later on. 
      self.val.getsetup() + "call void @print-pyret-number(" 
        + "%struct.pyret-number* " + self.val.getexpr() + ")\n"
    end
  | h-let(id :: String, val :: HExpr, body :: HStmt) with:
    tostring(self):
      "%" + self.id + " = alloca %struct.pyret-number*\n" + self.val.getsetup()
        + "store %struct.pyret-number* " + self.val.getexpr() 
        + ", %struct.pyret-number** %" + self.id + "\n" + self.body.tostring()
    end
  | h-var(id :: String, val :: HExpr, body :: HStmt) with:
    tostring(self):
      type = "%struct.pyret-number*"
      "%" + self.id + " = alloca " + type + "\n" + self.val.getsetup()
        + "store " + type + " " + self.val.getexpr()
        + ", " + type + "* %" + self.id + "\n" + self.body.tostring()
    end
    # TODO turn "var" into let v (new box) (set-box v value (...))
  | h-assign(id :: String, val :: HExpr, body :: HStmt) with:
    tostring(self):
      type = "%struct.pyret-number*"
      self.val.getsetup() + "store " + type + " " + self.val.getexpr()
       + ", " + type + "* %" + self.id + "\n" + self.body.tostring()
    end
    # TODO turn "assign" into set-box v value (...)
end

# This is the top-level H type. 
data HProg:
  # TODO this will also need to hold imports as well, eventually. 
  | h-prog(body :: HStmt, nums :: Set<Number>) with:
    tostring(self):
	  var str = H.get-file-text("runtime/num.ll") + "\n"

      # TODO declare the numbers that we will need
      # TODO they will be stored as variables called @num.v{num}
      str := for fold(s from str, n from self.nums.to-list()):
        str-n = n.tostring-fixed(10)
        s + num-prefix + str-n + " = private unnamed_addr constant ["
          + (str-n.length() + 1).tostring() + " x i8] c\"" 
          + str-n + "\\00\"\n"
      end

	  str := str + "define i64 @main() {\n"

      str := for fold(s from str, n from self.nums.to-list()):
        str-n = n.tostring-fixed(10)
        s + num-id-prefix + str-n + " = "
          + "call %struct.pyret-number* @initialize-integer(i8* getelementptr "
          + "inbounds ([" + (str-n.length() + 1).tostring() 
          + " x i8]* " + num-prefix + str-n + ", i32 0, i32 0))\n"
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
  | let-sub(id :: String, val :: HExpr)
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
# in H. Just create locals at the beginning of the run.
# Ultimately, we will be able to switch to an internal representation that
# doesn't even contain literals for "primatives" at all --- only identifiers.
fun filter-lets(prog :: HStmt) -> HStmt:
  fun lookup-in-subst(s :: String, 
                      subs :: List<Subst>, 
                      alt :: HExpr) -> HExpr:
    cases (List<Subst>) subs: 
      | link(f, r) => if f.id == s: f.val else: lookup-in-subst(s, r) end
      | empty => alt
    end
  end

  fun eval-const(expr :: HExpr) -> Option<HExpr>:
    cases (HExpr) expr:
      | h-func(_, _) => none
      | h-id-var(_, _) => none
      | h-app(_, _, _) => none
      | else => some(expr)
    end
  end

  fun filter-lets-expr(expr :: HExpr, subs :: List<Subst>) -> HExpr:
    cases (HExpr) expr: 
      | h-func(params, body) => 
          # This will get more complicated when we bring environments into the
          # picture. We will need to store the variables in our substitution
          # here, since they will be in this function's closure. 
          h-func(params, filter-lets-stmt(body, subs))
      | h-id(id, tmp) => lookup-in-subst(id, subs, expr)
      | h-app(f, args, tmp) => 
          h-app(filter-lets-expr(f, subs), 
                   for map(a from args): filter-lets-expr(a, subs) end,
                   tmp)
      | else => expr
    end
  end

  fun filter-lets-stmt(stmt :: HStmt, subs :: List<Subst>) -> HStmt:
    cases (HStmt) stmt: 
      | h-ret(val) => h-ret(filter-lets-expr(val, subs))
      | h-let(id, val, body) => 
          cases (Option<HExpr>) eval-const(filter-lets-expr(val, subs)):
            | none => h-let(id, val, filter-lets-stmt(body, subs))
            | some(c) => 
                newsubs = for map(s from subs): 
                            if is-h-id(s.val) and (s.val.id == id):
                              let-sub(s.id, c)
                            else:
                              s
                            end
                          end
                filter-lets-stmt(body, [let-sub(id, c)] + newsubs)
          end
      | h-var(id, val, body) => 
          h-var(id, 
                   filter-lets-expr(val, subs), 
                   filter-lets-stmt(body, subs))
      | h-assign(id, val, body) => 
          h-assign(id,
                      filter-lets-expr(val, subs),
                      filter-lets-stmt(body, subs))
    end
  end

  filter-lets-stmt(prog, empty)
where:
  filter-lets(h-ret(h-num(1, "tmp"))) is h-ret(h-num(1, "tmp"))
  filter-lets(
      h-let("x", 
               h-num(0, "tmp"), 
               h-ret(h-id("x", "tmp")))
    ) is h-ret(h-num(0, "tmp"))
end


# Lift Numbers
fun lift-nums(prog :: HStmt) -> Set<Number>:
  # Helper for lift-nums
  fun lift-nums-expr(expr :: HExpr) -> Set<Number>:
    cases (HExpr) expr:
      | h-num(n) => set([n])
      | h-func(params, body) => lift-nums-stmt(body)
      | h-app(f, args) => 
          for fold(base from lift-nums-expr(f), s from args):
            base.union(lift-nums-expr(s))
          end
      | else => set([])
    end
  end

  fun lift-nums-stmt(stmt :: HStmt) -> Set<Number>:
    cases (HStmt) stmt:
      | h-ret(val) => lift-nums-expr(val)
      | h-let(id, val, body) => 
          lift-nums-expr(val).union(lift-nums-stmt(body))
      | h-var(id, val, body) => 
          lift-nums-expr(val).union(lift-nums-stmt(body))
      | h-assign(id, val, body) => 
          lift-nums-expr(val).union(lift-nums-stmt(body))
    end
  end

  lift-nums-stmt(prog)
where:
  lift-nums(h-ret(h-num(3, "sdf"))) is set([3])
  lift-nums(h-let("v", 
                     h-num(4, "sdf"), 
                     h-ret(h-id("v", "sdf")))) is set([4])
end


# This function is basically the same as lift-nums, but lifts strings instead.
fun lift-strings(prog :: HStmt) -> Set<String>:
  fun lift-strings-expr(expr :: HExpr) -> Set<String>:
    cases (HExpr) expr: 
      | h-str(s) => set([s])
      | h-func(params, body) => lift-strings-stmt(body)
      | h-app(f, args) => 
          for fold(base from lift-strings-expr(f), s from args): 
            base.union(lift-strings-expr(s))
          end
      | else => set([])
    end
  end

  fun lift-strings-stmt(stmt :: HStmt) -> Set<String>:
    cases (HStmt) stmt: 
      | h-ret(val) => lift-strings-expr(val)
      | h-let(id, val, body) => 
          lift-strings-expr(val).union(lift-strings-stmt(body))
      | h-var(id, val, body) =>
          lift-strings-expr(val).union(lift-strings-stmt(body))
      | h-assign(id, val, body) => 
          lift-strings-expr(val).union(lift-strings-stmt(body))
    end
  end

  lift-strings-stmt(prog)

  # TODO add tests!
end


# This function is basically the same as lift-nums, but lifts bools instead.
fun lift-bools(prog :: HStmt) -> Set<Bool>:
  fun lift-bools-expr(expr :: HExpr) -> Set<Bool>:
    cases (HExpr) expr: 
      | h-bool(b) => set([b])
      | h-func(params, body) => lift-bools-stmt(body)
      | h-app(f, args) => 
          for fold(base from lift-bools-expr(f), s from args): 
            base.union(lift-bools-expr(s))
          end
      | else => set([])
    end
  end

  fun lift-bools-stmt(stmt :: HStmt) -> Set<Bool>:
    cases (HStmt) stmt: 
      | h-ret(val) => lift-bools-expr(val)
      | h-let(id, val, body) => 
          lift-bools-expr(val).union(lift-bools-stmt(body))
      | h-var(id, val, body) =>
          lift-bools-expr(val).union(lift-bools-stmt(body))
      | h-assign(id, val, body) => 
          lift-bools-expr(val).union(lift-bools-stmt(body))
    end
  end

  lift-bools-stmt(prog)

  # TODO add tests!
end


# This is the top-level function that will be called on ANF'ed code. It takes
# as argument an AProg, and returns an HProg. 
fun aprog-h(prog :: N.AProg) -> HProg:  
  cases (N.AProg) prog: 
    | a-program(l, imports, body) => 
        # TODO handle compiling headers
        hexpr = aexpr-h(body)
        nums = lift-nums(llexpr)

        # TODO be sure to call all transformational functions

        # TODO also be sure to handle all headers
        h-prog(hexpr, nums)
  end
end


# Compiles an AExpr into H
# TODO try and reduce repeated code here? 
fun aexpr-h(expr :: N.AExpr) -> HStmt:
  cases (N.AExpr) expr:
    | a-let(l, bind, e, body) => 
        if N.is-a-assign(e):
          tmp = next-val()
          h-let(tmp, 
                   aval-h(e.value), 
                   h-assign(e.id, 
                               h-id(tmp), 
                               h-let(bind.id, 
							            h-id(tmp), 
										aexpr-h(body))))
        else:
          h-let(bind.id, alettable-h(e), aexpr-h(body))
        end
    | a-var(l, bind, e, body) => 
        if N.is-a-assign(e):
          tmp = next-val()
          h-let(tmp, 
                   aval-h(e.value), 
                   h-assign(e.id, 
                               h-id(tmp), 
                               h-var(bind.id, 
							            h-id(tmp), 
										aexpr-h(body))))
        else:
          h-var(bind.id, alettable-h(e), aexpr-h(body))
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
          h-let(tmp, 
                   aval-h(e.value), 
                   h-assign(e.id, h-id(tmp), h-ret(alettable-h(e))))
        else:
          h-ret(alettable-h(e)) # TODO really? 
        end
  end
end


fun alettable-h(lettable :: N.ALettable) -> HExpr:
  cases (N.ALettable) lettable:
    | a-data-expr(l, name, variants, shared) => 
        raise("No data expressions just yet")
    | a-assign(l, id, value) => 
        raise("ERROR - a-assigns should have been handled already.")
    | a-app(l, _fun, args) => 
        h-app(aval-h(_fun), 
                 for map(a from args): aval-h(a) end, 
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
        h-func(for map(a from args): a.id end, 
                  aexpr-h(body))
    | a-method(l, args, body) => 
        raise("No method declarations just yet")
    | a-val(v) => aval-h(v)
  end
end


fun aval-h(val :: N.AVal) -> HExpr:
  cases (N.AVal) val:
    | a-num(l, n) => h-num(n)
    | a-str(l, s) => h-str(s)
    | a-bool(l, b) => h-bool(b)
    | a-undefined(l) => h-undef
    | a-id(l, id) => h-id(id)
    | a-id-var(l, id) => h-id-var(id) 
    | a-id-letrec(l, id) => 
        raise("Not implemented yet") # TODO
  end
end


















