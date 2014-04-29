#lang pyret

provide *
import file as F
import ast as A
import "types.arr" as T
import "ast-common.arr" as AC
import "ast-anf.arr" as N
import "ast-h.arr" as AH
import "helpers.arr" as H
# import "llvm/llvm.arr" as L
# import "llvm/atomic.arr" as Atomic
# import "llvm/fcmp.arr" as Fcmp
# import "llvm/icmp.arr" as Icmp
# import "llvm/kind.arr" as Kind
# Functions, calls, numbers, methods, variables, assigns, stdout

builtin-functions = [ 
    "rational-plus-method", 
    "rational-minus-method", 
    "rational-times-method", 
    "rational-divide-method",
    "rational-equals-method",
    "rational-lte-method"
]
    

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

num-len = 10
num-id-prefix = "num.p"
fun number-identifier(n :: Number):
    num-id-prefix + n.tostring-fixed(num-len)
end

string-id-prefix = "str.p"
bool-id-true = "bool.p-true"
bool-id-false = "bool.p-false"

closure-field    = AC.c-field-name("closure.f")
funcptr-field    = AC.c-field-name("funcptr.f")


default-loc = A.loc("N/A", -1, -1)

###################
# Mutable Globals #
###################

var datas = []
var globals :: List<AC.Global> = []
var funcs = []



# TODO TODO TODO
# TODO TODO TODO Figure out the scoping problem. 
# TODO TODO TODO 

# Substitution
data Subst:
  | let-sub(id :: String, val :: AC.Bind)
end

fun filter-lets(prog :: AH.HExpr) -> AH.HExpr:

  # TODO make sure this function will work
  fun lookup-in-subst(s :: AC.Bind, subs :: List<Subst>) -> AC.Bind:
    cases (List<Subst>) subs: 
      | link(f, r) => if f.id == s.id: f.val else: lookup-in-subst(s, r) end
      | empty => s
    end
  end

  fun filter-lets-field(field :: AH.HField, subs :: List<Subst>) -> AH.HField:
    AH.h-field(field.name, lookup-in-subst(field.value, subs))
  end

  fun filter-lets-lettable(expr :: AH.HLettable, 
                           subs :: List<Subst>) -> AH.HLettable:
    cases (AH.HLettable) expr:
      | h-id(bind) => AH.h-id(lookup-in-subst(bind, subs))
      | h-box(bind) => AH.h-box(lookup-in-subst(bind, subs))
      | h-unbox(bind) => AH.h-unbox(bind)
      | h-lam(f, closure) =>
          # TODO this code will need to be updated. How do we do filtering
          # of lets, now that closures are stored elsewhere?

      #    clnew = for map(s from set([]), c from closure.to-list()):
      #      s.union(set([lookup-in-subst(c, subs)]))
      #    end

          # TODO March 29 also, if we haven't already, we need to make sure
          # that we are doing the scoping changes in functions as well. 

          fun handle-func-vs(fs :: List<AH.NamedFunc>) -> List<AH.NamedFunc>:
            cases (List<AH.NamedFunc>) fs:
              | link(first, rest) => 
                  if first.name == f:
                    link(AH.named-func(first.name,
                                    first.args,
                                    filter-lets-expr(first.body, subs, first.name),
                                    first.ret), 
                         rest)
                  else:
                    link(first, handle-func-vs(rest))
                  end
              | empty => empty
            end
          end

          funcs := handle-func-vs(funcs)
          AH.h-lam(f, closure)
      | h-app(f, args) => 
          AH.h-app(lookup-in-subst(f, subs), 
                for map(arg from args): lookup-in-subst(arg, subs) end)
      | h-obj(fields) => 
          AH.h-obj(for map(f from fields): filter-lets-field(f, subs) end)
      | h-update(obj, fields) => 
          AH.h-update(lookup-in-subst(obj, subs), 
                   for map(f from fields): filter-lets-field(f, subs) end)
      | h-extend(obj, fields) => 
          AH.h-extend(lookup-in-subst(obj, subs), 
                   for map(f from fields): filter-lets-field(f, subs) end)
      | h-dot(obj, field) => 
          AH.h-dot(lookup-in-subst(obj, subs), field)
      | h-colon(obj, field) => 
          AH.h-colon(lookup-in-subst(obj, subs), field)
      | h-get-bang(obj, field) => 
          AH.h-get-bang(lookup-in-subst(obj, subs), field)
      | else => expr 
    end
  end

  fun filter-lets-expr(expr :: AH.HExpr, 
                       subs :: List<Subst>,
                       scope :: String) -> AH.HExpr:
    cases (AH.HExpr) expr: 
      | h-ret(bind) => AH.h-ret(lookup-in-subst(bind, subs))
      | h-let(bind, val, body) =>
          # TODO update this branch
          nval = filter-lets-lettable(val, subs)
          # TODO TODO TODO nval needs to be a type check. Function call?
          cases (AH.HLettable) nval:
            | h-id(id) => 
                filter-lets-expr(body, 
                                 link(let-sub(bind.id, id), 
                                      subs), 
                                 scope)
            | else => 
                AH.h-let(bind, nval, filter-lets-expr(body, subs, scope))
          end
          # if the let includes an actual type check (not any or blank),
          # we will leave it in, since that will be valid LLVM (probably 
          # requiring a phi node). 
      | h-assign(bind, val, body) => 
          # There will be no need to substitute into the left-hand side
          # of an assign expression. 
          AH.h-assign(bind, 
                      lookup-in-subst(val, subs), # TODO right?
                      filter-lets-expr(body, subs, scope))
      | h-try(body, bind, _except) => 
          # TODO I think we do need to replace "bind".
          new-id = scope + bind.id
          new-bind = bind.rename(new-id)
          AH.h-try(filter-lets-expr(body, subs, next-scope()),
                new-bind,
                filter-lets-expr(_except, 
                                 link(let-sub(bind.id, new-bind),
                                      subs), 
                                 next-scope()))
      | h-if(c, t, e) => 
          newcond = lookup-in-subst(c, subs)
          AH.h-if(newcond, 
               filter-lets-expr(t, subs, next-scope()), 
               filter-lets-expr(e, subs, next-scope()))
      | h-cases(type, val, branches, _else) =>
        new-val = lookup-in-subst(val, subs)
        branches-filtered = for map(branch from branches):
          AH.h-cases-branch(branch.name, branch.args, filter-lets-expr(branch.body, subs, scope))
        end
        _else-filtered = cases(Option<AH.HExpr>) _else:
          | some(e) => some(filter-lets-expr(e, subs, scope))
          | none    => none
        end
        AH.h-cases(type, new-val, branches-filtered, _else-filtered)
    end
  end

  filter-lets-expr(prog, empty, next-scope())
where:
  filter-lets(AH.h-ret("tmp")) is AH.h-ret("tmp")
  filter-lets(
      AH.h-let(AC.c-bind-loc(error.location("", -1, -1), "x", T.t-blank), 
            AH.h-id("tmp"), 
            AH.h-ret("x"))
    ) is AH.h-ret("tmp")
end

fun filter-func(func :: AH.NamedFunc) -> AH.NamedFunc:
  cases(AH.NamedFunc) func:
    | named-func(name, args, body, ret, is-closure) =>
      AH.named-func(name, args, filter-lets(body), ret, is-closure)
  end
end

  # Another utility function. We may want to raise it out of this scope. 
fun lookup-bind(id :: String, binds :: List<AC.Bind>) -> AC.Bind: 
  cases (List<AC.Bind>) binds: 
    | link(f, r) => if f.id == id: f else: lookup-bind(id, r) end
    | empty => raise("ID \"" + id + "\" is not bound.")
  end
end

fun get-free-vars(ex :: AH.HExpr, alrdy :: Set<String>) -> Set<String>:
  fun check-merge(v :: String, already :: Set<String>) -> Set<String>:
    if already.member(v): set([]) else: set([v]) end
  end

  fun gfv-expr(expr :: AH.HExpr, already :: Set<String>) -> Set<String>:
    cases (AH.HExpr) expr: 
      | h-ret(bind) => check-merge(bind.id, already)
      | h-let(bind, val, body) => 
          nalready = already.union(set([bind.id]))
          gfv-lettable(val, already).union(gfv-expr(body, nalready))
      | h-assign(bind, val, body) => 
        a = check-merge(bind.id, already)  
        b = check-merge(val.id, already)  
        gfv-expr(body, already).union(a).union(b)
      | h-try(body, bind, _except) => 
          nalready = already.union(set([bind.id]))
          gfv-expr(body, already).union(gfv-expr(_except, nalready))
      | h-if(c, t, e) => 
          s = check-merge(c.id, already).union(gfv-expr(t, already))
          s.union(gfv-expr(e, already))
      | h-cases(_, val, branches, _else) =>
        for fold(current from check-merge(val.id, already), branch from branches):
          current.union(cases(AH.HCasesBranch) branch:
            | h-cases-branch(name, args, body) =>
              gfv-expr(body, already.union(set(for map(arg from args): arg.id end)))
          end)
        end
    end
  end

  fun gfv-fields(fields :: List<AH.HField>, 
                 already :: Set<String>) -> Set<String>:
    for fold(s from set([]), f from fields):
      s.union(check-merge(f.value.id, already))
    end
  end

  fun gfv-lettable(lettable :: AH.HLettable, 
                   already :: Set<String>) -> Set<String>:
    cases (AH.HLettable) lettable: 
      | h-id(bind) => check-merge(bind.id, already)
      | h-box(bind) => check-merge(bind.id, already)
      | h-unbox(bind) => check-merge(bind.id, already)
      | h-lam(f, closure) => check-merge(closure.id, already) # TODO do we need this? 
      | h-app(func, args) => 
          for fold(s from check-merge(func.id, already), arg from args):
            s.union(check-merge(arg.id, already))
          end
      | h-obj(fields) => gfv-fields(fields, already)
      | h-update(obj, fields) => 
          check-merge(obj.id, already).union(gfv-fields(fields, already))
      | h-extend(obj, fields) => 
          check-merge(obj.id, already).union(gfv-fields(fields, already))
      | h-dot(obj, field) => check-merge(obj.id, already)
      | h-colon(obj, field) => check-merge(obj.id, already)
      | h-get-bang(obj, field) => check-merge(obj.id, already)
      | else => set([])
    end
  end

  gfv-expr(ex, alrdy)
end

fun let-lettable(bind :: AC.Bind, 
                 e :: N.ALettable, 
                 b :: N.AExpr,
                 vs :: Set<String>, 
                 binds :: List<AC.Bind>) -> AH.HExpr:

  # Declaring this function here, since it will be used multiple times
  # Both lists must be the same length (this is not checked!).
  # Also, depends on values of b and vs that it closes over, rather than
  # trying to pass them as arguments. 
  fun obj-fold(fields :: List<AField>, 
               done :: List<AH.HField>,
               finish :: (List<AH.HField> -> AH.HLettable)) -> AH.HExpr:
    cases (List<AField>) fields:
      | link(f, r) => 
          tmp-bind = AC.c-bind(next-val(), T.t-blank)
          field-name = AC.c-field-name(f.name)
          AH.h-let(tmp-bind,
                aval-h(f.value, vs), 
                obj-fold(r, link(AH.h-field(field-name, tmp-bind), done), finish))
      | empty => 
          # TODO reversing the list should not matter. 
          # I am just putting it there for reassurance. 
          AH.h-let(bind, finish(done.reverse()), aexpr-h(b, vs, binds))
    end
  end

  # this is like obj-fold, but for function calls. 
  fun app-fold(args :: List<AVal>,
               done :: List<AC.Bind>,
               finish :: (List<AC.Bind> -> AH.HLettable)) -> AH.HExpr:
    cases (List<AVal>) args:
      | link(f, r) => 
          tmp-bind = AC.c-bind(next-val(), T.t-blank)
          AH.h-let(tmp-bind,
                aval-h(f, vs),
                app-fold(r, link(tmp-bind, done), finish))
      | empty => 
          AH.h-let(bind, finish(done.reverse()), aexpr-h(b, vs, binds))
    end
  end

  # This is the main match statement of let-lettable. 
  cases (N.ALettable) e: 
    | a-data-expr(l, name, variants, shared) =>
        # variables to close over
        var data-tmps = []
        var data-vals = []

        # Handle fields in "variants"
        conv-variants = for map(dv from variants):
          var vtmps = []
          var vvals = []
          vfields = for map(wm from dv.with-members):
            tmp = next-val()
            tmp-bind = AC.c-bind(tmp, T.t-blank)
            vtmps := [tmp] + vtmps
            vvals := [aval-h(wm.value, vs)] + vvals
            AH.h-field(AC.c-field-name(wm.name), tmp-bind)
          end

          data-tmps := vtmps + data-tmps
          data-vals := vvals + data-vals

          cases (N.AVariant) dv:
            | a-variant(vl, vname, vmembers, vwith-members) =>
                AH.h-variant(vname, vmembers, vfields)
            | a-singleton-variant(vl, vname, vwith-members) => 
                AH.h-singleton-variant(vname, vfields)
          end
        end

        # Handle fields in "shared"
        conv-shared = for map(sf from shared): 
          tmp = next-val()
          tmp-bind = AC.c-bind(tmp)
          data-tmps := [tmp] + data-tmps 
          data-vals := [aval-h(sf.value, vs)] + data-vals
          AH.h-field(AC.c-field-name(sf.name), tmp-bind)
        end

        datas := [AH.named-data(name, conv-variants, conv-shared, set([]))] + datas

        # Note: lists *must* be the same length!
        fun do-let-bindings(dvars :: List<String>,
                            dvals :: List<AH.HLettable>,
                            body :: N.AExpr) -> AH.HExpr:
          cases (List<String>) dvars: 
            | link(f, r) => AH.h-let(AC.c-bind(f, T.t-blank),
                                  dvals.first, 
                                  do-let-bindings(r, dvals.rest, body))
            | empty => aexpr-h(body, vs, binds)
                # AH.h-let(bind, 
                #    AH.h-data(name, set(data-tmps)), 
                 #   aexpr-h(body, vs, binds))
          end
        end

        do-let-bindings(data-tmps.reverse(), data-vals.reverse(), b) 
    | a-assign(l, id, val) => 
        AH.h-let(bind, 
              aval-h(val, vs), 
              AH.h-assign(lookup-bind(id, binds), 
                       bind, 
                       aexpr-h(b, vs, binds)))
    | a-app(l, f, args) => 
        value = aval-h(f, vs)
        cases(AH.HLettable) value:
          | h-id(id) =>
            app-fold(args, empty, fun(fargs :: List<AC.Bind>) -> AH.HLettable:
              AH.h-app(id, fargs)
            end)
          | else     =>
            raise("Applying something that isn't an ID? This doesn't seem right....")
        end
    | a-help-app(l, f, args) => 
        raise("Congratulations! You've created an a-help-app!")
    | a-obj(l, fields) => 
        obj-fold(fields, empty, AH.h-obj)
    | a-update(l, obj, fields) => 
        tmp = next-val()
        AH.h-let(AC.c-bind(tmp, T.t-blank),
                 aval-h(obj, vs),
                 obj-fold(fields, 
                          empty, 
                          fun(flds :: List<AH.HField>) -> AH.HLettable:
                            AH.h-update(AC.c-bind(tmp, T.t-blank), flds)
                          end))
    | a-extend(l, obj, fields) => 
        tmp = next-val()
        AH.h-let(AC.c-bind(tmp, T.t-blank),
                 aval-h(obj, vs),
                 obj-fold(fields, 
                          empty, 
                          fun(flds :: List<AH.HField>) -> AH.HLettable:
                            AH.h-extend(AC.c-bind(tmp, T.t-blank), flds)
                          end))
    | a-dot(l, obj, field) => 
        tmp = next-val()
        AH.h-let(AC.c-bind(tmp, T.t-blank), 
                 aval-h(obj, vs), 
                 AH.h-let(bind, 
                          AH.h-dot(AC.c-bind(tmp, T.t-blank), 
                                   AC.c-field-name(field)), 
                          aexpr-h(b, vs, binds)))
    | a-colon(l, obj, field) =>
        tmp = next-val()
        AH.h-let(AC.c-bind(tmp, T.t-blank),
                 aval-h(obj, vs),
                 AH.h-let(bind, 
                          AH.h-colon(AC.c-bind(tmp, T.t-blank), 
                                     AC.c-field-name(field)), 
                          aexpr-h(b, vs, binds)))
    | a-get-bang(l, obj, field) => 
        tmp = next-val()
        AH.h-let(AC.c-bind(tmp, T.t-blank),
                 aval-h(obj, vs),
                 AH.h-let(bind, 
                          AH.h-get-bang(AC.c-bind(tmp, T.t-blank), 
                                        AC.c-field-name(field)), 
                          aexpr-h(b, vs, binds)))
    | a-lam(l, args, ret, body) =>
        name = gensym("func." + bind.id)
        name-bind = AC.c-bind-loc(l, name, T.t-blank)
        fbody = aexpr-h(body, vs, binds)

        # Determine if closure
		fvars = get-free-vars(fbody,
                              set(for map(a from args):
                                    a.id
                                  end + builtin-functions)).to-list()
        is-closure = fvars.length() <> 0

        # Lift procedure
        func = AH.named-func(name, args, fbody, ret, is-closure)
        funcs := link(func, funcs)
        if (is-closure):
          AH.h-let(bind, AH.h-id(name-bind), aexpr-h(b, vs, binds))
        else:
          new-body = for fold(base from fbody, vid from fvars):
            AH.h-let(AC.c-bind(vid, T.t-blank), AH.h-env(AC.c-field-name(vid)), base)
          end
		  tmp = AC.c-bind(next-val(), T.t-blank)
          closure-obj = AH.h-obj(for map(vid from fvars):
            field-name  = AC.c-field-name(vid)
            field-value = AC.c-bind(vid, T.t-blank)
            AH.h-field(field-name, field-value)
          end)
		  AH.h-let(tmp, closure-obj,
            AH.h-let(bind, AH.h-lam(name-bind, tmp), aexpr-h(b, vs, binds)))
        end
    | a-method(l, args, ret, body) =>
        raise("methods not currently handled")
    | a-val(v) => 
        AH.h-let(bind, aval-h(v, vs), aexpr-h(b, vs, binds))
  end
end

fun aexpr-h(expr :: N.AExpr, 
            vs :: Set<String>, 
            binds :: List<AC.Bind>) -> AH.HExpr:
  cases (N.AExpr) expr: 
    | a-let(l, bind, e, body) => 
      new-binds = link(bind, binds)
      let-lettable(bind, e, body, vs, new-binds)
    | a-var(l, bind, e, body) =>
        tmp = next-val()
        tmp-bind = AC.c-bind(tmp, T.t-blank)
        new-binds = link(bind, link(tmp-bind, binds))
        if N.is-a-val(e):
          AH.h-let(tmp-bind,
                aval-h(e.v, vs),
                AH.h-let(bind, 
                      AH.h-box(tmp-bind),
                      aexpr-h(body, vs, new-binds)))
        else:
          let-lettable(tmp-bind,
                       e,
                       N.a-var(l, 
                               bind, 
                               N.a-val(N.a-id(default-loc, tmp)), 
                               body), 
                               new-binds)
        end
    | a-try(l, body, b, _except) =>  
        AH.h-try(aexpr-h(body, vs, binds), b, aexpr-h(_except, vs, binds))
    | a-split-app(l, is-var, f, args, helper, helper-args) => 
        raise("Congratulations! You've generated an a-split-app!")
    | a-if(l, c, t, e) => 
        tmp-bind = AC.c-bind(next-val(), T.t-blank)
        AH.h-let(tmp-bind, 
              aval-h(c, vs), 
              AH.h-if(tmp-bind, 
                   aexpr-h(t, vs, binds), 
                   aexpr-h(e, vs, binds)))
    | a-cases(l, type, val, branches, _else) =>
      hv = aval-h(val, vs)
      hv-bind = AC.c-bind-loc(l, next-val(), type)
      new-else = cases(Option<AExpr>) _else:
        | some(e) => some(aexpr-h(e, vs, binds))
        | none    => none
      end
      new-branches = for map(branch from branches):
        cases(N.ACasesBranch) branch:
          | a-cases-branch(_, name, args, body) =>
            AH.h-cases-branch(name, args, aexpr-h(body, vs, binds + args))
        end
      end
      AH.h-let(hv-bind, hv, AH.h-cases(type, hv-bind, new-branches, new-else))
    | a-lettable(e) => 
        cases (N.ALettable) e:
          | a-val(v) => 
              tmp = AC.c-bind(next-val(), T.t-blank)
              AH.h-let(tmp, aval-h(v, vs), AH.h-ret(tmp))
          | else =>
              tmp = next-val()
              let-lettable(AC.c-bind(tmp, T.t-blank),
                           e,
                           N.a-lettable(N.a-val(N.a-id(default-loc, tmp))),
                           vs,
                           binds)
        end
  end
end

fun afield-h(field :: N.AField) -> AH.HField:
  cases (N.AField) field: 
    | a-field(l, name, value) => AH.h-field(name, aval-h(value))
  end
end

fun avariant-h(variant :: N.AVariant) -> AH.HVariant:
  cases (N.AVariant) variant: 
    | a-variant(l, name, members, with-members) => 
        AH.h-variant(name, members, map(afield-h, with-members))
    | a-singleton-variant(l, name, with-members) =>
        AH.h-singleton-variant(name, map(afield-h, with-members))
  end
end

fun aval-h(val :: N.AVal, vars :: Set<String>) -> AH.HLettable:
  cases (N.AVal) val: 
    | a-num(l, n) => 
      num-bind = AC.c-bind-loc(l, number-identifier(n), T.t-number)
      globals := link(AC.c-num(num-bind, n), globals)
      AH.h-id(num-bind)
    | a-str(l, s) => 
        str-name = AC.c-bind-loc(l, gensym("str.p"), T.t-name("String"))
        globals := link(AC.c-str(str-name, s), globals)
        AH.h-id(str-name)
    | a-bool(l, b) => 
        if b: AH.h-id(bool-id-true) else: AH.h-id(bool-id-false) end
    | a-undefined(l) => AH.h-undefined
    | a-id(l, id) => 
      id-bind = AC.c-bind-loc(l, id, T.t-blank)
      if vars.member(id): AH.h-unbox(id-bind) else: AH.h-id(id-bind) end 
    | a-id-var(l, id) =>
        raise("ERROR - a-id-var should have been removed by now")
    | a-id-letrec(l, id) => 
        raise("ERROR - a-id-letrec should have been removed by now")
  end
end


# This is the top-level function that will be called on ANF'ed code. It takes
# as argument an AProg, and returns an HProg.
# We might want to consider doing some of the things that we do in here 
# (such as lifting everything) in some other function. However, that is a 
# decision that can be made later, since it will require moving only a few 
# lines of code. 
fun anf-to-h(prog :: N.AProg):  
  funcs := []
  datas := []
  globals := []
  cases (N.AProg) prog: 
    | a-program(l, imports, body) => # TODO
        fbody = filter-lets(aexpr-h(body, set([]), empty))
        {globals : globals,
         datas : datas,
         funcs : funcs.map(filter-func),
         expr : fbody}
  end
end
