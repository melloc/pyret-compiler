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

num-prefix = "@num.v"

num-id-prefix = "%num.p"
string-id-prefix = "%str.p"
bool-id-true = "%bool.p-true"
bool-id-false = "%bool.p-false"

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
  | h-alloca with: 
    tosyn(self): "alloca()" end
  | h-id(id :: String) with:
    tosyn(self): self.id end # TODO something else? 
  | h-lookup(id :: String) with: 
    tosyn(self): "lookup " + self.id end
  | h-data(name :: String, Set<String>)
  | h-lam(f :: String, closure :: Set<String>)
  | h-app(f :: String, args :: List<String>)
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
    tosyn(self): "ret " + id end
  | h-let(bind :: N.ABind, val :: HLettable, body :: HExpr) with: 
    tosyn(self): 
      self.bind.id + " : " + self.bind.ann + " = " + self.val.tosyn() 
        + "\n" + self.body.tosyn() 
    end
  | h-assign(bind :: N.ABind, val :: String, body :: HExpr) 
  | h-try(body :: HExpr, bind :: N.ABind, _except :: HExpr)
  | h-if(c :: String, t :: HExpr, e :: HExpr)
end

# This is the top-level H type. 
data HProg:
  # TODO this will also need to hold imports as well, eventually. 
  | h-prog(body :: HStmt, nums :: Set<Number>)
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

#  fun eval-const(expr :: HLettable) -> Option<String>:
#    cases (HLettable) expr:
#      | h-func(_, _) => none
#      | h-id-var(_, _) => none
#      | h-app(_, _, _) => none
#      | else => some(expr)
#    end
#  end

  fun filter-lets-field(field :: HField, subs :: List<Subst>) -> HField:
    h-field(field.name, lookup-in-subst(field.value, subs))
  end

  fun filter-lets-lettable(expr :: HLettable, subs :: List<Subst>) -> HLettable:
    cases (HLettable) expr:
      | h-id(id) => h-id(lookup-in-subst(id, subs))
      | h-lookup(id) => # TODO do we replace this one? Probably not...
      | h-data # TODO not sure...
      | h-lam(f, closure) => 
          clnew = for map(s from set([]), c from closure.to-list()):
            s.union(set([lookup-in-subst(c, subs)]))
          end

          fun handle-func-vs(funcs :: List<NamedFunc>) -> List<NamedFunc>:
            cases (List<NamedFunc>) funcs:
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

          funcs = handle-func-vs(funcs)
          h-lam(f, clnew)
      | h-app(f, args) => 
          h-app(lookup-in-subst(f, subs), 
                for map(a from args): lookup-in-subst(a, subs) end)
      | h-obj(fields) => 
          h-obj(for map(f from fields): filter-lets-field(f, subs) end
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
    cases (HStmt) stmt: 
      | h-ret(val) => h-ret(filter-lets-expr(val, subs))
      | h-let(bind, val, body) =>
          nval = filter-lets-lettable(val, subs)
          cases (HLettable) nval:
            | h-id(id) => 
                if bind.ann == N.a_any or bind.ann == N.a_blank:
                  filter-lets-expr(body, link(let-sub(bind.id, id), subs))
                else: 
                  h-let(bind, nval, fitler-lets-expr(body, subs))
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
      | h-if(c, t, e) => 
          newcond = lookup-in-subst(c, subs, c.id)
          h-if(newcond, filter-lets-expr(t, subs), filter-lets-expr(e, subs))
    end
  end

  filter-lets-stmt(prog, empty)
where:
  filter-lets(h-ret(h-id("tmp"))) is h-ret(h-id("tmp"))
  filter-lets(
      h-let(N.a_bind(error.location("", -1, -1), "x", N.a_blank), 
            h-id("tmp"), 
            h-ret(h-id("x")))
    ) is h-ret(h-id("tmp"))
end



# Internal data type for representing data expressions
data NamedData:
  | named-data(name :: String, 
               variants :: List<HVariant>, 
               shared :: List<HField>)
end

# and other types necessary for data expressions: 
data HVariant: 
  | h-variant(name :: String, 
              members :: List<N.AVariantMember>, 
              with-members :: List<HField>)
  | h-singleton-variant(name :: String, 
                        with-members :: List<HField>)
  # TODO why do we need two variants here? Why not just have the list
  # empty to represent the second type? 
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


# This will be the function that we eventually call. It will do just about 
# everything that we need from this phase, and also define and call the 
# functions that will convert Joe's ANF representation into our halfway
# representation. 
# h-let now takes binds rather than Strings; that will be the case whereever
# it is in Joe's code as well.
# TODO this should be moved to the top; no more wrapping in a function. 



next-func-name = (fun():
  var n = 0
  fun() -> String: 
    n := n + 1
    "@func.p" + n.tostring()
  end
end)()

  # Another utility function. We may want to raise it out of this scope. 
fun lookup-bind(id :: String, binds :: List<N.ABind>) -> N.ABind: 
  cases (N.ABind) binds: 
    | link(f, r) => if f.id == id: f else: lookup-bind(id, r) end
    | empty => raise("ID \"" + id "\" is not bound.")
  end
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
          h-let(N.a-bind(tmp, N.a_blank),
                aval-h(f.value, vs), 
                obj-fold(r, link(h-field(f.name, tmp), done)))
      | empty => 
          # TODO reversing the list should not matter. 
          # I am just putting it there for reassurance. 
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
            | link(f, r) => h-let(N.a-bind(f, N.a_blank),
                                  dvals.first, 
                                  do-let-bindings(r, dvals.rest, body))
            | empty => h-let(bind, h-data(name), aexpr-h(body, vs, binds))
          end
        end

        do-let-bindings(data-tmps.reverse(), data-vals.reverse(), b)

        # TODO when filtering lets, we need to make sure that we look up 
        # each data expression in the list, and traverse it if necessary. 
    | a-assign(l, id, val) => 
        h-let(bind, 
              aval-h(val, vs), 
              h-assign(lookup-bind(id, binds), 
                       bind.id, 
                       aexpr-h(b, vs, binds)))
    | a-app(l, f, args) => # TODO this is why we're doing it this way...
    | a-help-app(l, f, args) => # TODO what is this?  
    | a-obj(l, fields) => 
        obj-fold(fields, empty, h-obj)
    | a-update(l, super, fields) => 
        obj-fold(fields, 
                 empty, 
                 fun(flds :: List<HField>) -> HLettable:
                   h-update(super, flds)
                 end)
    | a-extend(l, super, fields) => 
        obj-fold(fields, 
                 empty, 
                 fun(flds :: List<HField>) -> HLettable:
                   h-extend(super, flds)
                 end)
    | a-dot(l, obj, field) => 
        tmp = next-val()
        h-let(N.a-bind(tmp, N.a_blank), 
              aval-h(obj, vs), 
              h-let(bind, h-dot(tmp, field), aexpr-h(b, vs, binds)))
    | a-colon(l, obj, field) =>
        tmp = next-val()
        h-let(N.a-bind(tmp, N.a_blank),
              aval-h(obj, vs),
              h-let(bind, h-colon(tmp, field), aexpr-h(b, vs, binds)))
    | a-get-bang(l, obj, field) => 
        tmp = next-val()
        h-let(N.a-bind(tmp, N.a_blank),
              aval-h(obj, vs),
              h-let(bind, h-get-bang(tmp, field), aexpr-h(b, vs, binds)))
    | a-lam(l, args, ret, body) =>
        name = next-func-name()
        fbody = aexpr-h(body, vs, binds)
        closure = get-free-vars(fbody)
        funcs := link(named-func(name, args, ret, fbody, closure), funcs)
        h-let(bind, h-lam(name, closure), aexpr-h(b, vs, binds))
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
              h-alloca,
              aexpr-h(a-let(l, 
                            N.a-bind(tmp, N.a_blank), 
                            e, 
                            a-let(l,
                                  N.a-bind(next-val(), N.a_blank), 
                                  a-assign(l, bind, a-id(tmp)), 
                                  body)),
                      vs.union(set([bind.id])),
                      binds))
        # TODO this last one is a bit complex, and may not work. 
        # It needs serious testing before we should trust it. 
    | a-try(l, body, b, _except) => 
        # TODO make sure this is correct. 
        h-try(aexpr-h(body, vs, binds), b, aexpr-h(_except, vs, binds))
    | a-split-app(l, is-var, f, args, helper, helper-args) => 
        
        # TODO


    | a-if(l, c, t, e) => 
        tmp = next-val()
        h-let(N.a-bind(tmp, N.a_blank), 
              aval-h(c, vs), 
              h-if(tmp, 
                   aexpr-h(t, vs, binds), 
                   aexpr-h(e, vs, binds)))
    | a-lettable(e) => 
        cases (N.ALettable) e:
          | a-val(v) => 
              tmp = next-val()
              h-let(N.a-bind(tmp, N.a_blank), aval-h(v, vs), h-ret(tmp))
          | else =>
              tmp = next-val()
              let-lettable(N.a-bind(tmp, N.a_blank),
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
        strings := link(s, strings)
        # TODO figure out how to represent string as ID
        str-name = ""
        h-id(str-name)
    | a-bool(l, b) => 
        #bools := link(b, bools)
        # TODO let's just always initialize true and false, okay?
        if b: h-id(bool-id-true) else: h-id(bool-id-false) end
    | a-undefined(l) => h-undefined
    | a-id(l, id) => if vars.member(id): h-lookup(v) else: h-id(id) end
        # TODO figure out what to do here. We probably want to get the 
        # set of free variables in a function using a different, recursive
        # function, which will be defined later. 
    | a-id-var(l, id) =>
        raise("ERROR - a-id-var should have been removed by now")
    | a-id-letrec(l, id) => 
        raise("ERROR - a-id-letrec should have been removed by now")
  end
end

  # TODO we need a new filter-lets function. When it hits a function 
  # declaration, it will either (A) go find that function in the list
  # of functions to lift, and filter its body, or (B) store the current
  # set of substitutions in a list with the function's name as a key, 
  # and then we will continue the substituting later. I think the former
  # will be more straightforward to implement; I just don't want to do
  # something that will overwhelm memory if the program is large.  
  # I will need to remember to also substitute in closure sets, both in
  # the h-lam objects, and in the named-function objects. 



  # This is the very end
  # Be sure to set the ID program-insides to the value of the program!

  # TODO wrap this object creation in a function. That function, and only
  # that function, will be exported to the other files. 
  {"nums" : set(nums), 
   "strings" : set(strings),  
   "datas" : set(datas), 
   "funcs" : set(funcs),
   "expr" : program-insides}





# Convert Variant
#fun avariant-h(variant :: N.AVariant) -> HVariant:
#  cases (N.AVariant) variant: 
#    | a-variant(l, name, members, with-members) => 
#        h-variant(name, 
#                  for map(m from members): avariantmember-h(m) end,
#                  for map(w from with-members): afield-h(w) end)
#    | a-singleton-variant(l, name, with-members) => 
#        h-singleton-variant(name, 
#                            for map(w from with-members): afield-h(w) end)
#  end
#end

# Convert Variant Member
#fun avariantmember-h(member :: AVariantMember) -> HVariantMember:
#  h-variant-member(member.member-type, 
#                   h-bind(member.bind.id, member.bind.ann))
#end

# Convert Field
#fun afield-h(field :: N.AField) -> HField:
#  h-field(field.name, field.value)
#end



# This is the top-level function that will be called on ANF'ed code. It takes
# as argument an AProg, and returns an HProg.
# We might want to consider doing some of the things that we do in here 
# (such as lifting everything) in some other function. However, that is a 
# decision that can be made later, since it will require moving only a few 
# lines of code. 
fun aprog-h(prog :: N.AProg) -> HProg:  
  cases (N.AProg) prog: 
    | a-program(l, imports, body) => 
        # TODO we also need to lift data declarations
    #    datas = lift-datas(body)

        # TODO handle compiling headers
     #   hexpr = aexpr-h(remove-datas(body))
    #    nums = lift-nums(hexpr)
   #     strings = lift-strings(hexpr)
  #      bools = lift-bools(hexpr)
##        rexpr = replace-literals(hexpr)

        # TODO be sure to call all transformational functions

        # TODO also be sure to handle all headers
        #h-prog(rexpr, nums)
        # TODO h-prog(hexpr, nums, strings, bools, funcs, datas, headers)
  end
end


# Compiles an AExpr into H
# TODO try and reduce repeated code here? 
#fun aexpr-h(expr :: N.AExpr) -> HStmt:
#  cases (N.AExpr) expr:
#    | a-let(l, bind, e, body) => 
#        if N.is-a-assign(e):
#          tmp = next-val()
#          h-let(tmp, 
#                   aval-h(e.value), 
#                   h-assign(e.id, 
#                               h-id(tmp), 
#                               h-let(bind.id, 
#							            h-id(tmp), 
#										aexpr-h(body))))
#        else:
#          h-let(bind.id, alettable-h(e), aexpr-h(body))
#        end
#    | a-var(l, bind, e, body) => 
#        if N.is-a-assign(e):
#          tmp = next-val()
#          h-let(tmp, 
#                   aval-h(e.value), 
#                   h-assign(e.id, 
#                               h-id(tmp), 
#                               h-var(bind.id, 
#							            h-id(tmp), 
#										aexpr-h(body))))
#        else:
#          h-var(bind.id, alettable-h(e), aexpr-h(body))
#        end
#    | a-try(l, body, b, _except) => 
#        raise("No try blocks just yet")
#    | a-split-app(l, is-var, f, args, helper, helper-args) => 
#        raise("Just what *is* an a-split-app?")
#    | a-if(l, c, t, e) => 
#        raise("No conditionals just yet")
#    | a-lettable(e) =>
#        if N.is-a-assign(e):
#          tmp = next-val()
#          h-let(tmp, 
#                   aval-h(e.value), 
#                   h-assign(e.id, h-id(tmp), h-ret(alettable-h(e))))
#        else:
#          h-ret(alettable-h(e)) # TODO really? 
#        end
#  end
#end


#fun alettable-h(lettable :: N.ALettable) -> HExpr:
#  cases (N.ALettable) lettable:
#    | a-data-expr(l, name, variants, shared) => 
#        raise("ERROR - a-data-exprs should have been handled already.")
#    | a-assign(l, id, value) => 
#        raise("ERROR - a-assigns should have been handled already.")
#    | a-app(l, _fun, args) => 
#        h-app(aval-h(_fun), 
#                 for map(a from args): aval-h(a) end, 
#                 next-val())
#    | a-help-app(l, f, args) => raise("Just what is an a-help-app?")
#    | a-obj(l, fields) => 
#        h-obj(for map(f from fields): afield-h(f) end)
#    | a-update(l, super, fields) => 
#        raise("No object updates just yet")
#    | a-extend(l, super, fields) => 
#        raise("No object extensions just yet")
#    | a-dot(l, obj, field) =>
#        raise("No object field accesses just yet")
#    | a-colon(l, obj, field) => 
#        raise("No object colon accesses just yet")
#    | a-get-bang(l, obj, field) => 
#        raise("No object bang accesses just yet")
#    | a-lam(l, args, body) => 
#        h-func(for map(a from args): a.id end, 
#                  aexpr-h(body))
#    | a-method(l, args, body) => 
#        raise("No method declarations just yet")
#    | a-val(v) => aval-h(v)
#  end
#end


#fun aval-h(val :: N.AVal) -> HExpr:
#  cases (N.AVal) val:
#    | a-num(l, n) => h-num(n)
#    | a-str(l, s) => h-str(s)
#    | a-bool(l, b) => h-bool(b)
#    | a-undefined(l) => h-undef
#    | a-id(l, id) => h-id(id)
#    | a-id-var(l, id) => h-id-var(id) 
#    | a-id-letrec(l, id) => 
#        raise("Not implemented yet") # TODO
#  end
#end


















