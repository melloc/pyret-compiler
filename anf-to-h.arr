#lang pyret

provide *
import file as F
import ast as A
import "ast-anf.arr" as N
import "ast-h.arr" as AH
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

closure-arg-id = "%closure.p"
closure-field-id = "closure.f"
funcptr-field-id = "funcptr.f"

num-len = 10


###################
# Mutable Globals #
###################

var datas = []
var nums = []
var strings = []
var funcs = []



# TODO TODO TODO
# TODO TODO TODO Figure out the scoping problem. 
# TODO TODO TODO 

# Substitution
data Subst:
  | let-sub(id :: String, val :: String)
end

fun to-hbind(bind :: N.ABind) -> AH.Bind:
  cases(N.ABind) bind:
    | a-bind(l, name, ann) => AH.h-bind(name, ann)
  end
end

fun filter-lets(prog :: AH.HExpr) -> AH.HExpr:

  # TODO make sure this function will work
  fun lookup-in-subst(s :: String, subs :: List<Subst>) -> String:
    cases (List<Subst>) subs: 
      | link(f, r) => if f.id == s: f.val else: lookup-in-subst(s, r) end
      | empty => s
    end
  end

  fun filter-lets-field(field :: AH.HField, subs :: List<Subst>) -> AH.HField:
    AH.h-field(field.name, lookup-in-subst(field.value, subs))
  end

  fun filter-lets-lettable(expr :: AH.HLettable, 
                           subs :: List<Subst>) -> AH.HLettable:
    cases (AH.HLettable) expr:
      | h-id(id) => AH.h-id(lookup-in-subst(id, subs))
      | h-box(id) => AH.h-box(lookup-in-subst(id, subs))
      | h-unbox(id) => AH.h-unbox(id) # really? 
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
                for map(a from args): lookup-in-subst(a, subs) end)
      | h-obj(fields) => 
          AH.h-obj(for map(f from fields): filter-lets-field(f, subs) end)
      | h-update(super, fields) => 
          AH.h-update(lookup-in-subst(super, subs), 
                   for map(f from fields): filter-lets-field(f, subs) end)
      | h-extend(super, fields) => 
          AH.h-extend(lookup-in-subst(super, subs), 
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
      | h-ret(val) => AH.h-ret(lookup-in-subst(val, subs))
      | h-let(bind, val, body) =>
          # TODO update this branch
          nval = filter-lets-lettable(val, subs)
          cases (AH.HLettable) nval:
            | h-id(id) => 
                if (bind.ann == A.a_any) or (bind.ann == A.a_blank):
                  filter-lets-expr(body, 
                                   link(let-sub(bind.id, id), 
                                        subs), 
                                   scope)
                else:
                  new-id = scope + bind.id
                  new-bind = AH.h-bind(new-id, bind.ann) # TODO right?
                  AH.h-let(new-bind, 
                        nval, 
                        filter-lets-expr(body, 
                                         link(let-sub(bind.id, new-id), 
                                              subs), 
                                         scope))
                        # TODO check all of this twice. 
                end
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
                   lookup-in-subst(val, subs), 
                   filter-lets-expr(body, subs, scope))
      | h-try(body, bind, _except) => 
          # TODO I think we do need to replace "bind".
          new-id = scope + bind.id
          new-bind = AH.h-bind(new-id, bind.ann)
          AH.h-try(filter-lets-expr(body, subs, next-scope()),
                new-bind,
                filter-lets-expr(_except, 
                                 link(let-sub(bind.id, new-id),
                                      subs), 
                                 next-scope()))
      | h-if(c, t, e) => 
          newcond = lookup-in-subst(c, subs)
          AH.h-if(newcond, 
               filter-lets-expr(t, subs, next-scope()), 
               filter-lets-expr(e, subs, next-scope()))
    end
  end

  filter-lets-expr(prog, empty, next-scope())
where:
  filter-lets(AH.h-ret("tmp")) is AH.h-ret("tmp")
  filter-lets(
      AH.h-let(AH.h-bind(error.location("", -1, -1), "x", A.a_blank), 
            AH.h-id("tmp"), 
            AH.h-ret("x"))
    ) is AH.h-ret("tmp")
end



  # Another utility function. We may want to raise it out of this scope. 
fun lookup-bind(id :: String, binds :: List<AH.Bind>) -> AH.Bind: 
  cases (List<AH.Bind>) binds: 
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
      | h-ret(val) => check-merge(val, already)
      | h-let(bind, val, body) => 
          nalready = already.union(set([bind.id]))
          gfv-lettable(val, already).union(gfv-expr(body, nalready))
      | h-assign(bind, val, body) => 
        a = check-merge(bind.id, already)  
        b = check-merge(val, already)  
        gfv-expr(body, already).union(a).union(b)
      | h-try(body, bind, _except) => 
          nalready = already.union(set([bind.id]))
          gfv-expr(body, already).union(gfv-expr(_except, nalready))
      | h-if(c, t, e) => 
          s = check-merge(c, already).union(gfv-expr(t, already))
          s.union(gfv-expr(e, already))
    end
  end

  fun gfv-fields(fields :: List<AH.HField>, 
                 already :: Set<String>) -> Set<String>:
    for fold(s from set([]), f from fields):
      s.union(check-merge(f.value, already))
    end
  end

  fun gfv-lettable(lettable :: AH.HLettable, 
                   already :: Set<String>) -> Set<String>:
    cases (AH.HLettable) lettable: 
      | h-id(id) => check-merge(id, already)
      | h-box(id) => check-merge(id, already)
      | h-unbox(id) => check-merge(id, already)
      | h-lam(f, closure) => check-merge(closure, already) # TODO do we need this? 
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

fun let-lettable(bind :: AH.Bind, 
                 e :: N.ALettable, 
                 b :: N.AExpr,
                 vs :: Set<String>, 
                 binds :: List<AH.Bind>) -> AH.HExpr:

  # Declaring this function here, since it will be used multiple times
  # Both lists must be the same length (this is not checked!).
  # Also, depends on values of b and vs that it closes over, rather than
  # trying to pass them as arguments. 
  fun obj-fold(fields :: List<AField>, 
               done :: List<AH.HField>,
               finish :: (List<AH.HField> -> AH.HLettable)) -> AH.HExpr:
    cases (List<AField>) fields:
      | link(f, r) => 
          tmp = next-val()
          AH.h-let(AH.h-bind(tmp, A.a_blank),
                aval-h(f.value, vs), 
                obj-fold(r, link(AH.h-field(f.name, tmp), done)))
      | empty => 
          # TODO reversing the list should not matter. 
          # I am just putting it there for reassurance. 
          AH.h-let(bind, finish(done.reverse()), aexpr-h(b, vs, binds))
    end
  end

  # this is like obj-fold, but for function calls. 
  fun app-fold(args :: List<AVal>,
               done :: List<String>,
               finish :: (List<String> -> AH.HLettable)) -> AH.HExpr:
    cases (List<AVal>) args:
      | link(f, r) => 
          tmp = next-val()
          AH.h-let(AH.h-bind(tmp, A.a_blank),
                aval-h(f, vs),
                app-fold(r, link(tmp, done), finish))
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
            vtmps := [tmp] + vtmps
            vvals := [aval-h(wm.value, vs)] + vvals
            AH.h-field(wm.name, tmp)
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
          data-tmps := [tmp] + data-tmps 
          data-vals := [aval-h(sf.value, vs)] + data-vals
          AH.h-field(sf.name, tmp)
        end

        datas := [AH.named-data(name, conv-variants, conv-shared)] + datas

        # Note: lists *must* be the same length!
        fun do-let-bindings(dvars :: List<String>,
                            dvals :: List<AH.HLettable>,
                            body :: N.AExpr) -> AH.HExpr:
          cases (List<String>) dvars: 
            | link(f, r) => AH.h-let(AH.h-bind(f, A.a_blank),
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
                       bind.id, 
                       aexpr-h(b, vs, binds)))
    | a-app(l, f, args) => 
        tmp = next-val()
        tmp-closure = next-val()
        tmp-func = next-val()
        AH.h-let(AH.h-bind(tmp, A.a_blank),
              aval-h(f, vs),
              AH.h-let(AH.h-bind(tmp-closure, A.a_blank),
                    AH.h-dot(tmp, AH.h-bind(closure-field-id, A.a_blank)),
                    AH.h-let(AH.h-bind(tmp-func, A.a_blank),
                          AH.h-dot(tmp, AH.h-bind(funcptr-field-id, A.a_blank)),
                          app-fold(args, 
                                   empty,
                                   fun(fargs :: List<String>) -> AH.HLettable:
                                     AH.h-app(tmp-func, [tmp-closure] + fargs)
                                   end))))
    | a-help-app(l, f, args) => 
        raise("Congratulations! You've created an a-help-app!")
    | a-obj(l, fields) => 
        obj-fold(fields, empty, AH.h-obj)
    | a-update(l, super, fields) => 
        tmp = next-val()
        AH.h-let(AH.h-bind(tmp, A.a_blank),
              aval-h(super, vs),
              obj-fold(fields, 
                       empty, 
                       fun(flds :: List<AH.HField>) -> AH.HLettable:
                         AH.h-update(tmp, flds)
                       end))
    | a-extend(l, super, fields) => 
        tmp = next-val()
        AH.h-let(AH.h-bind(tmp, A.a_blank),
              aval-h(super, vs),
              obj-fold(fields, 
                       empty, 
                       fun(flds :: List<AH.HField>) -> AH.HLettable:
                         AH.h-extend(tmp, flds)
                       end))
    | a-dot(l, obj, field) => 
        tmp = next-val()
        AH.h-let(AH.h-bind(tmp, A.a_blank), 
              aval-h(obj, vs), 
              AH.h-let(bind, AH.h-dot(tmp, field), aexpr-h(b, vs, binds)))
    | a-colon(l, obj, field) =>
        tmp = next-val()
        AH.h-let(AH.h-bind(tmp, A.a_blank),
              aval-h(obj, vs),
              AH.h-let(bind, AH.h-colon(tmp, field), aexpr-h(b, vs, binds)))
    | a-get-bang(l, obj, field) => 
        tmp = next-val()
        AH.h-let(AH.h-bind(tmp, A.a_blank),
              aval-h(obj, vs),
              AH.h-let(bind, AH.h-get-bang(tmp, field), aexpr-h(b, vs, binds)))
    | a-lam(l, args, ret, body) =>
        name = next-func-name()
        fbody = aexpr-h(body, vs, binds)
		fvars = get-free-vars(fbody, 
                              set(for map(a from args):
                                    a.id
                                  end)).to-list()
        funcs := link(
          AH.named-func(name, 
                     [closure-arg-id] + args, 
                     for fold(base from fbody, vid from fvars):
                       AH.h-let(AH.h-bind(vid, A.a_blank),
                             AH.h-dot(closure-arg-id, AH.h-bind(vid, A.a_blank)),
                             base)
                     end, ret),
          funcs
        )

		tmp = next-val()
		AH.h-let(AH.h-bind(tmp, A.a_blank),
              AH.h-closure(for map(vid from fvars): AH.h-field(vid, vid) end),
			  AH.h-let(bind, AH.h-lam(name, tmp), aexpr-h(b, vs, binds)))
    | a-method(l, args, ret, body) =>
        name = next-func-name()
        fbody = aexpr-h(body, vs, binds)
		fvars = get-free-vars(fbody,
                              set(for map(a from args):
                                a.id
                              end)).to-list()
        funcs := link(
          AH.named-func(name, 
                     [closure-arg-id] + args, 
                     ret, 
                     for fold(base from fbody, vid from fvars):
                       AH.h-let(AH.h-bind(vid, A.a_blank),
                             AH.h-dot(closure-arg-id, AH.h-bind(vid, A.a_blank)),
                             base)
                     end),
          funcs
        )

		tmp = next-val()
		AH.h-let(AH.h-bind(tmp, A.a_blank),
              AH.h-closure(for map(vid from fvars): AH.h-field(vid, vid) end),
			  AH.h-let(bind, AH.h-lam(name, tmp), aexpr-h(b, vs, binds)))
    | a-val(v) => 
        AH.h-let(bind, aval-h(v, vs), aexpr-h(b, vs, binds))
  end
end

fun aexpr-h(expr :: N.AExpr, 
            vs :: Set<String>, 
            binds :: List<AH.Bind>) -> AH.HExpr:
  cases (N.AExpr) expr: 
    | a-let(l, bind, e, body) => 
      new-bind  = to-hbind(bind)
      new-binds = link(new-bind, binds)
      let-lettable(new-bind, e, body, vs, new-binds)
    | a-var(l, bind, e, body) =>
        tmp = next-val()
        tmp-bind = AH.h-bind(tmp, A.a_blank)
        new-bind = to-hbind(bind)
        new-binds = link(new-bind, link(tmp-bind, binds))
        if N.is-a-val(e):
          AH.h-let(tmp-bind,
                aval-h(e.v, vs),
                AH.h-let(new-bind, 
                      AH.h-box(tmp),
                      aexpr-h(body, vs, new-binds)))
        else:
          let-lettable(tmp-bind,
                       e,
                       N.a-var(l, bind, N.a-val(N.a-id(tmp)), body), new-binds)
        end
    | a-try(l, body, b, _except) =>  
        AH.h-try(aexpr-h(body, vs, binds), b, aexpr-h(_except, vs, binds))
    | a-split-app(l, is-var, f, args, helper, helper-args) => 
        raise("Congratulations! You've generated an a-split-app!")
    | a-if(l, c, t, e) => 
        tmp = next-val()
        AH.h-let(AH.h-bind(tmp, A.a_blank), 
              aval-h(c, vs), 
              AH.h-if(tmp, 
                   aexpr-h(t, vs, binds), 
                   aexpr-h(e, vs, binds)))
    | a-lettable(e) => 
        cases (N.ALettable) e:
          | a-val(v) => 
              tmp = next-val()
              AH.h-let(AH.h-bind(tmp, A.a_blank), aval-h(v, vs), AH.h-ret(tmp))
          | else =>
              tmp = next-val()
              let-lettable(AH.h-bind(tmp, A.a_blank),
                           e,
                           N.a-lettable(N.a-val(N.a-id(tmp))),
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
        nums := link(n, nums)
        AH.h-id(num-id-prefix + n.tostring-fixed(num-len))
    | a-str(l, s) => 
        str-name = next-string-name()
        strings := link(AH.named-str(str-name, s), strings)
        AH.h-id(str-name)
    | a-bool(l, b) => 
        if b: AH.h-id(bool-id-true) else: AH.h-id(bool-id-false) end
    | a-undefined(l) => AH.h-undefined
    | a-id(l, id) => if vars.member(id): AH.h-unbox(id) else: AH.h-id(id) end 
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
  cases (N.AProg) prog: 
    | a-program(l, imports, body) => # TODO
        fbody = filter-lets(aexpr-h(body, set([]), empty))
        {nums : nums,
         strings : strings,
         datas : datas,
         funcs : funcs,
         expr : fbody}
  end
end
