#lang pyret

provide *
import file as F
import ast as A
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
    "%id.v" + n.tostring()
  end
end)()

next-scope = (fun():
  var n = 0
  fun() -> String:
    n := n + 1
    "%s" + n.tostring() + "."
  end
end)()

next-func-name = (fun():
  var n = 0
  fun() -> String: 
    n := n + 1
    "@func.p" + n.tostring()
  end
end)()

next-string-name = (fun():
  var n = 0
  fun() -> String:
    n := n + 1
    "@str.p" + n.tostring()
  end
end)()

num-prefix = "@num.v"

num-id-prefix = "%num.p"
string-id-prefix = "%str.p"
bool-id-true = "%bool.p-true"
bool-id-false = "%bool.p-false"

closure-id = "%closure.p"

num-len = 10


###################
# Mutable Globals #
###################

var datas = []
var nums = []
var strings = []
var funcs = []



# Data definition for H constructs
# Let's have tostring() functions that print out a surface representation.
# We'll need some way of handling depth for that. We'll deal with that later. 
data HLettable:
  | h-undefined with: 
    tosyn(self): "undefined" end
  | h-box with: 
    tosyn(self): "box" end
  | h-id(id :: String) with:
    tosyn(self): self.id end # TODO something else? 
  | h-unbox(id :: String) with: 
    tosyn(self): "unbox " + self.id end
  | h-lam(f :: String, closure :: Set<String>) with:
    tosyn(self): self.f + "{" + self.closure.to-list().str-join(",") + "}" end
  | h-app(f :: String, args :: List<String>) with:
    tosyn(self): self.f + "(" + self.args.str-join(",") + ")" end
  | h-obj(fields :: List<HField>)
  | h-update(super :: String, fields :: List<HField>)
  | h-extend(super :: String, fields :: List<HField>) # TODO merge?
  | h-dot(obj :: String, field :: N.ABind)
  | h-colon(obj :: String, field :: N.ABind)
  | h-get-bang(obj :: String, field :: N.ABind) # more? 
end

# We are, for now, just saving the binds as such. 
data HExpr:
  | h-ret(id :: String) with:
    tosyn(self): "ret " + self.id end
  | h-let(bind :: N.ABind, val :: HLettable, body :: HExpr) with: 
    tosyn(self): 
      self.bind.id + " : " + self.bind.ann + " = " + self.val.tosyn() 
        + "\n" + self.body.tosyn() 
    end
  | h-assign(bind :: N.ABind, val :: String, body :: HExpr) 
  | h-try(body :: HExpr, bind :: N.ABind, _except :: HExpr)
  | h-if(c :: String, t :: HExpr, e :: HExpr)
end

# TODO TODO TODO
# TODO TODO TODO Figure out the scoping problem. 
# TODO TODO TODO 

# Substitution
data Subst:
  | let-sub(id :: String, val :: String)
end


fun filter-lets(prog :: HExpr) -> HExpr:

  # TODO make sure this function will work
  fun lookup-in-subst(s :: String, subs :: List<Subst>) -> String:
    cases (List<Subst>) subs: 
      | link(f, r) => if f.id == s: f.val else: lookup-in-subst(s, r) end
      | empty => s
    end
  end

  fun filter-lets-field(field :: HField, subs :: List<Subst>) -> HField:
    h-field(field.name, lookup-in-subst(field.value, subs))
  end

  fun filter-lets-lettable(expr :: HLettable, subs :: List<Subst>) -> HLettable:
    cases (HLettable) expr:
      | h-id(id) => h-id(lookup-in-subst(id, subs))
      | h-unbox(id) => h-unbox(id) # really? 
      | h-data(name, closure) => # TODO not sure...
      | h-lam(f, closure) => 
          clnew = for map(s from set([]), c from closure.to-list()):
            s.union(set([lookup-in-subst(c, subs)]))
          end

          fun handle-func-vs(fs :: List<NamedFunc>) -> List<NamedFunc>:
            cases (List<NamedFunc>) fs:
              | link(first, rest) => 
                  if first.name == f:
                    link(named-func(first.name,
                                    first.args,
                                    filter-lets-expr(first.body, subs),
                                    first.ret, 
                                    clnew), 
                         rest)
                  else:
                    link(first, handle-func-vs(rest))
                  end
              | empty => empty
            end
          end

          funcs := handle-func-vs(funcs)
          h-lam(f, clnew)
      | h-app(f, args) => 
          h-app(lookup-in-subst(f, subs), 
                for map(a from args): lookup-in-subst(a, subs) end)
      | h-obj(fields) => 
          h-obj(for map(f from fields): filter-lets-field(f, subs) end)
      | h-update(super, fields) => 
          h-update(lookup-in-subst(super, subs), 
                   for map(f from fields): filter-lets-field(f, subs) end)
      | h-extend(super, fields) => 
          h-extend(lookup-in-subst(super, subs), 
                   for map(f from fields): filter-lets-field(f, subs) end)
      | h-dot(obj, field) => 
          h-dot(lookup-in-subst(obj, subs), field)
      | h-colon(obj, field) => 
          h-colon(lookup-in-subst(obj, subs), field)
      | h-get-bang(obj, field) => 
          h-get-bang(lookup-in-subst(obj, subs), field)
      | else => expr 
    end
  end

  fun filter-lets-expr(expr :: HExpr, subs :: List<Subst>) -> HExpr:
    cases (HExpr) expr: 
      | h-ret(val) => h-ret(lookup-in-subst(val, subs))
      | h-let(bind, val, body) =>
          nval = filter-lets-lettable(val, subs)
          cases (HLettable) nval:
            | h-id(id) => 
                if (bind.ann == A.a_any) or (bind.ann == A.a_blank):
                  filter-lets-expr(body, link(let-sub(bind.id, id), subs))
                else: 
                  h-let(bind, nval, filter-lets-expr(body, subs))
                end
            | else => 
                h-let(bind, nval, filter-lets-expr(body, subs))
          end
          # if the let includes an actual type check (not any or blank),
          # we will leave it in, since that will be valid LLVM (probably 
          # requiring a phi node). 
      | h-assign(bind, val, body) => 
          # There will be no need to substitute into the left-hand side
          # of an assign expression. 
          h-assign(bind, 
                   lookup-in-subst(val, subs), 
                   filter-lets-expr(body, subs))
      | h-try(body, bind, _except) => # TODO
      | h-if(c, t, e) => 
          newcond = lookup-in-subst(c, subs, c.id)
          h-if(newcond, filter-lets-expr(t, subs), filter-lets-expr(e, subs))
    end
  end

  filter-lets-expr(prog, empty)
where:
  filter-lets(h-ret("tmp")) is h-ret("tmp")
  filter-lets(
      h-let(N.a-bind(error.location("", -1, -1), "x", A.a_blank), 
            h-id("tmp"), 
            h-ret("x"))
    ) is h-ret("tmp")
end



# Internal data type for representing data expressions
data NamedData:
  | named-data(name :: String, 
               variants :: List<HVariant>, 
               shared :: List<HField>,
               closure :: Set<String>)
end

# and other types necessary for data expressions: 
data HVariant: 
  | h-variant(name :: String, 
              members :: List<N.AVariantMember>, 
              with-members :: List<HField>)
  | h-singleton-variant(name :: String, 
                        with-members :: List<HField>) 
end

data HField:
  h-field(name :: String, value :: String)
end
# The value of a field can be a string, since we lift the lookup out 
# beforehand. If that lookup is in fact just an id, we will substitute
# it back in before generating code. 


data NamedFunc:
  | named-func(name :: String, 
               args :: List<N.ABind>, 
               body :: HExpr,
               ret :: A.Ann,
               closure :: Set<String>)
end

data NamedString:
  | named-str(name :: String, val :: String)
end


  # Another utility function. We may want to raise it out of this scope. 
fun lookup-bind(id :: String, binds :: List<N.ABind>) -> N.ABind: 
  cases (N.ABind) binds: 
    | link(f, r) => if f.id == id: f else: lookup-bind(id, r) end
    | empty => raise("ID \"" + id + "\" is not bound.")
  end
end

fun get-free-vars(ex :: HExpr, alrdy :: Set<String>) -> Set<String>:
  fun check-merge(v :: String, already :: Set<String>) -> Set<String>:
    if already.member(v): set([]) else: set([v]) end
  end

  fun gfv-expr(expr :: HExpr, already :: Set<String>) -> Set<String>:
    cases (HExpr) expr: 
      | h-ret(val) => check-merge(val)
      | h-let(bind, val, body) => 
          nalready = already.union(set([bind.id]))
          gfv-lettable(val, already).union(gfv-expr(body, nalready))
      | h-assign(bind, val, body) => 
          gfv-lettable(val, already).union(gfv-expr(body, already))
      | h-try(body, bind, _except) => 
          nalready = already.union(set([bind.id]))
          gfv-expr(body, already).union(gfv-expr(_except, nalready))
      | h-if(c, t, e) => 
          s = check-merge(c, already).union(gfv-expr(t, already))
          s.union(gfv-expr(e, already))
    end
  end

  fun gfv-fields(fields :: List<HField>, 
                 already :: Set<String>) -> Set<String>:
    for fold(s from set([]), f from fields):
      s.union(check-merge(f.value, already))
    end
  end

  fun gfv-lettable(lettable :: HLettable, 
                   already :: Set<String>) -> Set<String>:
    cases (HLettable) lettable: 
      | h-id(id) => check-merge(id, already)
      | h-unbox(id) => check-merge(id, already)
      | h-lam(f, closure) => closure
      | h-app(func, args) => 
          for fold(s from check-merge(func, already), a from args):
            s.union(check-merge(a, already))
          end
      | h-obj(fields) => gfv-fields(fields, already)
      | h-update(super, fields) => 
          check-merge(super, already).union(gfv-fields(fields, already))
      | h-extend(super, fields) => 
          check-merge(super, already).union(gfv-fields(fields, already))
      | h-dot(obj, field) => check-merge(obj, already)
      | h-colon(obj, field) => check-merge(obj, already)
      | h-get-bang(obj, field) => check-merge(obj, already)
      | else => set([])
    end
  end

  gfv-expr(ex, alrdy)
end

fun let-lettable(bind :: N.ABind, 
                 e :: N.ALettable, 
                 b :: N.AExpr,
                 vs :: Set<String>, 
                 binds :: List<N.ABind>) -> HExpr:

  # Declaring this function here, since it will be used multiple times
  # Both lists must be the same length (this is not checked!).
  # Also, depends on values of b and vs that it closes over, rather than
  # trying to pass them as arguments. 
  fun obj-fold(fields :: List<AField>, 
               done :: List<HField>,
               finish :: (List<HField> -> HLettable)) -> HExpr:
    cases (List<AField>) fields:
      | link(f, r) => 
          tmp = next-val()
          h-let(N.a-bind(tmp, A.a_blank),
                aval-h(f.value, vs), 
                obj-fold(r, link(h-field(f.name, tmp), done)))
      | empty => 
          # TODO reversing the list should not matter. 
          # I am just putting it there for reassurance. 
          h-let(bind, finish(done.reverse()), aexpr-h(b, vs, binds))
    end
  end

  # this is like obj-fold, but for function calls. 
  fun app-fold(args :: List<AVal>,
               done :: List<String>,
               finish :: (List<String> -> HLettable)) -> HExpr:
    cases (List<String>) args:
      | link(f, r) => 
          tmp = next-val()
          h-let(N.a-bind(tmp, A.a_blank),
                aval-h(f, vs),
                app-fold(r, link(tmp, done)))
      | empty => 
          h-let(bind, finish(done.reverse()), aexpr-h(b, vs, binds))
    end
  end

  # This is the main match statement of let-lettable. 
  cases (N.ALettable) e: 
    | a-data-expr(l, name, variants, shared) =>
        # TODO lift things

        # variables to close over
        var data-tmps = []
        var data-vals = []

        # Handle fields in "variants"
        conv-variants = for map(dv from variants):
          var vtmps = []
          var vvals = []
          vfields = for map(wm from dv.with-members):
            tmp = next-val()
            vtmps := [tmp] + vtmps
            vvals := [aval-h(wm.value, vs)] + vvals
            h-field(wm.name, tmp)
          end

          data-tmps := vtmps + data-tmps
          data-vals := vvals + data-vals

          cases (N.AVariant) dv:
            | a-variant(vl, vname, vmembers, vwith-members) =>
                h-variant(vname, vmembers, vfields)
            | a-singleton-variant(vl, vname, vwith-members) => 
                h-singleton-variant(vname, vfields)
          end
        end

        # Handle fields in "shared"
        conv-shared = for map(sf from shared): 
          tmp = next-val()
          data-tmps := [tmp] + data-tmps 
          data-vals := [aval-h(sf.value, vs)] + data-vals
          h-field(sf.name, tmp)
        end

        datas := [named-data(name, conv-variants, conv-shared)] + datas

        # Note: lists *must* be the same length!
        fun do-let-bindings(dvars :: List<String>,
                            dvals :: List<HLettable>,
                            body :: N.AExpr) -> HExpr:
          cases (List<String>) dvars: 
            | link(f, r) => h-let(N.a-bind(f, A.a_blank),
                                  dvals.first, 
                                  do-let-bindings(r, dvals.rest, body))
            | empty => aexpr-h(body, vs, binds)
                # h-let(bind, 
                    h-data(name, set(data-tmps)), 
                    aexpr-h(body, vs, binds))
          end
        end

        do-let-bindings(data-tmps.reverse(), data-vals.reverse(), b) 
    | a-assign(l, id, val) => 
        h-let(bind, 
              aval-h(val, vs), 
              h-assign(lookup-bind(id, binds), 
                       bind.id, 
                       aexpr-h(b, vs, binds)))
    | a-app(l, f, args) => 
        tmp = next-val()
        h-let(N.a-bind(tmp, A.a_blank),
              aval-h(f, vs),
              app-fold(args, 
                       empty,
                       fun(arguments :: List<String>) -> HLettable:
                         h-app(tmp, arguments)
                       end))
    | a-help-app(l, f, args) => 
        raise("Congratulations! You've created an a-help-app!")
    | a-obj(l, fields) => 
        obj-fold(fields, empty, h-obj)
    | a-update(l, super, fields) => 
        tmp = next-val()
        h-let(N.a-bind(tmp, A.a_blank),
              aval-h(super, vs),
              obj-fold(fields, 
                       empty, 
                       fun(flds :: List<HField>) -> HLettable:
                         h-update(tmp, flds)
                       end))
    | a-extend(l, super, fields) => 
        tmp = next-val()
        h-let(N.a-bind(tmp, A.a_blank),
              aval-h(super, vs),
              obj-fold(fields, 
                       empty, 
                       fun(flds :: List<HField>) -> HLettable:
                         h-extend(tmp, flds)
                       end))
    | a-dot(l, obj, field) => 
        tmp = next-val()
        h-let(N.a-bind(tmp, A.a_blank), 
              aval-h(obj, vs), 
              h-let(bind, h-dot(tmp, field), aexpr-h(b, vs, binds)))
    | a-colon(l, obj, field) =>
        tmp = next-val()
        h-let(N.a-bind(tmp, A.a_blank),
              aval-h(obj, vs),
              h-let(bind, h-colon(tmp, field), aexpr-h(b, vs, binds)))
    | a-get-bang(l, obj, field) => 
        tmp = next-val()
        h-let(N.a-bind(tmp, A.a_blank),
              aval-h(obj, vs),
              h-let(bind, h-get-bang(tmp, field), aexpr-h(b, vs, binds)))
    | a-lam(l, args, ret, body) =>
        name = next-func-name()
        fbody = aexpr-h(body, vs, binds)
        # TODO we also need to change the body to bind everything that was
        # inside of that closure. It would probably be a good idea to just 
        # assume that it is the same as an object. Do for methods to.
        funcs := link(
          named-func(name, 
                     [closure-id] + args, 
                     ret, 
                     for fold(base from fbody,
                              vid from get-free-vars(fbody).to-list()):
                       h-let(N.a-bind(vid, A.a_blank),
                             h-dot(closure-id, N.a-bind(vid, A.a_blank)),
                             base)
                     end),
          funcs
        )
        h-let(bind, h-lam(name), aexpr-h(b, vs, binds))
    | a-method(l, args, ret, body) =>
        name = next-func-name()
        fbody = aexpr-h(body, vs, binds)
        closure = get-free-vars(fbody)
        funcs := link(named-func(name, args, ret, fbody, closure), funcs)
        h-let(bind, h-lam(name, closure), aexpr-h(b, vs, binds))
    | a-val(v) => 
        h-let(bind, aval-h(v, vs), aexpr-h(b, vs, binds))
  end
end

fun aexpr-h(expr :: N.AExpr, 
            vs :: Set<String>, 
            binds :: List<N.ABind>) -> HExpr:
  cases (N.AExpr) expr: 
    | a-let(l, bind, e, body) => let-lettable(bind, e, body, vs, binds)
    | a-var(l, bind, e, body) =>
        tmp = next-val()
        h-let(bind, 
              h-box,
              aexpr-h(N.a-let(l, 
                              A.a-bind(tmp, A.a_blank), 
                              e, 
                              N.a-let(l,
                                      N.a-bind(next-val(), A.a_blank), 
                                      N.a-assign(l, bind, N.a-id(tmp)), 
                                      body)),
                      vs.union(set([bind.id])),
                      binds))
        # TODO this last one is a bit complex, and may not work. 
        # It needs serious testing before we should trust it. 
    | a-try(l, body, b, _except) =>  
        h-try(aexpr-h(body, vs, binds), b, aexpr-h(_except, vs, binds))
    | a-split-app(l, is-var, f, args, helper, helper-args) => 
        raise("Congratulations! You've generated an a-split-app!")
    | a-if(l, c, t, e) => 
        tmp = next-val()
        h-let(N.a-bind(tmp, A.a_blank), 
              aval-h(c, vs), 
              h-if(tmp, 
                   aexpr-h(t, vs, binds), 
                   aexpr-h(e, vs, binds)))
    | a-lettable(e) => 
        cases (N.ALettable) e:
          | a-val(v) => 
              tmp = next-val()
              h-let(N.a-bind(tmp, A.a_blank), aval-h(v, vs), h-ret(tmp))
          | else =>
              tmp = next-val()
              let-lettable(N.a-bind(tmp, A.a_blank),
                           e,
                           N.a-lettable(N.a-val(N.a-id(tmp))),
                           vs,
                           binds)
        end
  end
end

fun afield-h(field :: N.AField) -> HField:
  cases (N.AField) field: 
    | a-field(l, name, value) => h-field(name, aval-h(value))
  end
end

fun avariant-h(variant :: N.AVariant) -> HVariant:
  cases (N.AVariant) variant: 
    | a-variant(l, name, members, with-members) => 
        h-variant(name, members, map(afield-h, with-members))
    | a-singleton-variant(l, name, with-members) =>
        h-singleton-variant(name, map(afield-h, with-members))
  end
end

fun aval-h(val :: N.AVal, vars :: Set<String>) -> HLettable:
  cases (N.AVal) val: 
    | a-num(l, n) => 
        nums := link(n, nums)
        h-id(num-id-prefix + n.tostring-fixed(num-len))
    | a-str(l, s) => 
        str-name = next-string-name()
        strings := link(named-str(str-name, s), strings)
        h-id(str-name)
    | a-bool(l, b) => 
        if b: h-id(bool-id-true) else: h-id(bool-id-false) end
    | a-undefined(l) => h-undefined
    | a-id(l, id) => if vars.member(id): h-unbox(id) else: h-id(id) end 
    | a-id-var(l, id) =>
        raise("ERROR - a-id-var should have been removed by now")
    | a-id-letrec(l, id) => 
        raise("ERROR - a-id-letrec should have been removed by now")
  end
end


# TODO wrap this object creation in a function. That function, and only
# that function, will be exported to the other files. 
#  {"nums" : set(nums), 
#   "string" : set(strings),  
#   "datas" : set(datas), 
#   "funcs" : set(funcs),
#   "expr" : program-insides}


# TODO the above object should be created and returned by the aprog-h function.



# This is the top-level function that will be called on ANF'ed code. It takes
# as argument an AProg, and returns an HProg.
# We might want to consider doing some of the things that we do in here 
# (such as lifting everything) in some other function. However, that is a 
# decision that can be made later, since it will require moving only a few 
# lines of code. 
fun aprog-h(prog :: N.AProg):  
  cases (N.AProg) prog: 
    | a-program(l, imports, body) => # TODO
  end
end



