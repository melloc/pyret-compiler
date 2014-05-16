#lang pyret

provide *

import "types.arr" as T
import "ast-common.arr" as AC
import "helpers.arr" as H
import "ast-h.arr" as AH


data Constraint:
  #| sub-con(lhs :: T.Type, rhs :: T.Type)
  | eq-con(lhs :: T.Type, rhs :: T.Type)
end

fun sub-con(l :: T.Type, r :: T.Type) -> Constraint:
  eq-con(l, r)
end

fun is-sub-con(s) -> Boolean:
  false
end

debug = false

#data Term:
#  | t-expr(val :: AH.HExpr)
#  | t-lettable(val :: AH.HLettable)
#  | t-id(val :: String)
#  | t-var(val :: String)
#  | t-ty(val :: T.Type)
#  | t-box(val :: Term)
#  | t-arrow(args :: List<Term>, ret :: Term)
##  | t-record(fields :: List<TermField>)
##  | t-lookup(obj :: Term, field :: String)
#sharing:
##  tostring(self):
##    cases (Term) self:
#      | t-expr(val) => "expr(" + val._torepr() + ")"
#      | t-lettable(val) => "lettable(" + val._torepr() + ")"
#      | t-id(val) => "id(" + val.tostring() + ")"
#      | t-var(val) => "var(" + val.tostring() + ")"
#      | t-ty(val) => "ty(" + val.tostring() + ")"
#      | t-box(val) => "box(" + val.tostring() + ")"
#      | t-arrow(args, ret) => 
#          "(" + args.join-str(",") + ") -> " + ret.tostring()
#      | t-record(fields) => "{" + fields.join-str(",") + "}"
#      | t-lookup(obj, field) => obj.tostring() + "." + field
#    end
#  end
#end

##data TermField:
#  | t-field(name :: String, type :: Term)
#end

data Substitution:
  | let-sub(lhs :: T.Type, rhs :: T.Type)
end


#fun type-to-term(t :: T.Type) -> Term:
#  cases (T.Type) t:
#    | t-arrow(args, ret) => 
#        t-arrow(args.map(type-to-term), type-to-term(ret))
#    | t-method(args, ret) => 
#        t-arrow(args.map(type-to-term), type-to-term(ret))
#    | t-record(fields) => 
#        t-record(for map(f from fields): 
#                   t-field(f.name, type-to-term(f.type)) 
#                 end)
##    | t-pointer(ty) => t-box(type-to-term(ty))
#    | else => t-ty(t)
#  end
#end


fun next-tvar() -> String:
  gensym("tvar.")
end


# some(true) if it is a subtype; some(false) if not, and none
# if it is impossible to determine. 
fun is-subtype(lhs :: T.Type, rhs :: T.Type) -> Option<Boolean>:
  cases (T.Type) lhs:
    | t-expr(_) => # TODO 
    | t-lettable(_) => # TODO
    | t-id(_) => # TODO
    | t-var(_) => # TODO
    | t-name(name) => 
    | t-pointer(ty) => # TODO
    | t-arrow(args, ret) => # TODO
    | t-method(args, ret) => 
    | t-record(fields) => # TODO 
    | t-lookup(obj, field) => # TODO
    | t-void => 
    | t-byte => 
    | t-word => 
    | t-number => 
    | t-any => 
    | t-blank => 
    | t-param-name(_, _) => raise("ERROR: t-param-name!")
  end
end


fun typecheck-failed(l :: T.Type, r :: T.Type):
  raise("Typecheck failed: expected" + l.tostring() + "; got " + r.tostring())
end


# NOTE: only for bind *lookups*. 
fun cg-bind(bind :: AC.Bind) -> List<Constraint>:
  cases (T.Type) bind.ty:
    | t-blank => [eq-con(T.t-id(bind.id), T.t-var(bind.id))]
    | else => [eq-con(T.t-id(bind.id), bind.ty),
               sub-con(T.t-var(bind.id), bind.ty)] # TODO sub, or eq? 
  end
end


fun cg-expr(expr :: AH.HExpr, 
            datas :: List<AH.NamedData>,
            funcs :: List<AH.NamedFunc>) -> List<Constraint>:
  cases (AH.HExpr) expr:
    | h-ret(id) => cg-bind(id)
     #   cases (T.Type) id.ty:
     #     | t-blank => [eq-con(t-id(id.id), t-var(id.id))]
     #     | else => [eq-con(T.t-id(id.id), id.ty),
     #                eq-con(T.t-var(id.id), id.ty)]
      #  end
    | h-let(bind, val, body) => 
        cases (T.Type) bind.ty:
          | t-blank => [eq-con(T.t-var(bind.id), T.t-lettable(val))]
          | else => [eq-con(T.t-var(bind.id), bind.ty),
                     sub-con(T.t-lettable(val), bind.ty)]
        end
          + cg-expr(body, datas, funcs) + cg-lettable(val, funcs) 
          + [eq-con(T.t-expr(expr), T.t-expr(body))]
    | h-assign(bind, val, body) =>
        cases (T.Type) bind.ty:
          | t-blank => 
              cases (T.Type) val.ty:
                | t-blank => [sub-con(T.t-id(val.id), T.t-var(bind.id))]
                | else => [sub-con(val.ty, T.t-var(bind.id))]
              end
          | t-pointer(ty) => 
              cases (T.Type) val.ty:
                | t-blank => 
                    when ty == T.T-blank:
                      print("1234")
                    end
                    [sub-con(T.t-id(val.id), ty)]
                | else => 
                    when not (ty == val.ty):
                      raise("Typecheck failed: " + ty.tostring()
                              + " vs " + val.ty.tostring())
                    end
                    []
              end
          | else => raise("Assignment can only be done to a pointer")
        end
          + cg-expr(body, datas, funcs) 
          + [sub-con(T.t-expr(expr), T.t-expr(body))] + cg-bind(val)
    | h-try(body, bind, _except) => 
        cases (T.Type) bind.ty:
          | t-blank => []
                       # TODO not sure what to do here. 
                       # We *could* look through and find the raise type
                       # as well as the return type, but that would be hard. 
          | else => [eq-con(T.t-var(bind.id), bind.ty)]
        end
          + cg-expr(body, datas, funcs) + cg-expr(_except, datas, funcs)
          + [sub-con(T.t-expr(body), T.t-expr(expr)),
             sub-con(T.t-expr(_except), T.t-expr(expr))]
    | h-if(c, t, e) => 
        cases (T.Type) c.ty:
          | t-blank => [sub-con(T.t-id(c.id), T.t-var(c.id))]
          | else => [sub-con(T.t-id(c.id), c.ty)]
        end
          + cg-expr(t, datas, funcs) + cg-expr(e, datas, funcs) 
          + [sub-con(T.t-expr(t), T.t-expr(expr)),
             sub-con(T.t-expr(e), T.t-expr(expr))] # is this an infinite loop?
    | h-cases(type, val, branches, _else) => 
        when not T.is-t-name(type):
          raise("cases only works with named types. ")
        end
        
        dat = cases (Option<AH.NamedData>) datas.find(
                           fun(nd :: AH.NamedData) -> Bool: 
                             nd.name == type.name 
                           end):
          | none => raise("Not valid type for cases statement: " + type.name)
          | some(s) => s
        end

        # TODO TODO TODO TODO
        # TODO TODO TODO TODO
        # TODO TODO TODO TODO
        # TODO TODO TODO TODO 
        cases (T.Type) val.ty:
          | t-blank => [sub-con(T.t-id(val.id), type)]
          | else => 
              when not (val.ty == type):
                raise("Type mismatch in cases value")
              end
        end
         + for fold(s from [], b from branches):
             s + cases (AH.HCasesBranch) b:
               | h-cases-branch(name, args, body) =>
                  opvar = dat.get-variant(name)

                  when is-none(opvar):
                    raise("Variant " + name + " not found for type " 
                           + type.tostring())
                  end

                  cases (AH.HVariant) opvar.value:
                    | h-variant(_, members, _) => 
                        for map2(a from args, m from members):
                          cases (T.Type) a.ty:
                            | t-blank => sub-con(T.t-id(a.id), m.bind.ty)
                            | else =>
                                when not (a.ty == m.bind.ty):
                                  raise("Type mismatch in cases binding")
                                end
                                sub-con(T.t-id(a.id), a.ty)
                          end
                        end
                    | h-singleton-variant(_, _) => 
                        # Check for number of args
                        when not is-empty(args):
                          raise("singleton variant takes no args")
                        end
                        []
                  end
                   + cg-expr(body, datas, funcs) 
                   + [sub-con(T.t-expr(body), T.t-expr(expr))]
             end
           end
         + cases (Option<AH.HExpr>) _else:
             | none => []
             | some(s) => 
                 cg-expr(s, datas, funcs) + [sub-con(T.t-expr(s), T.t-expr(expr))]
           end 
  end
end


fun get-field(rec :: List<T.TypeField>, name :: String) -> Option<T.Type>:
  cases (List<T.TypeField>) rec: 
    | empty => none
    | link(f, r) => 
        if f.name == name: some(f.type) else: get-field(r, name) end
  end
end


fun cg-lettable(lettable :: AH.HLettable, 
                funcs :: List<AH.NamedFunc>) -> List<Constraint>:
  cases (AH.HLettable) lettable:
    | h-undefined => [] # I think this is right...
    | h-box(id) =>
        cases (T.Type) id.ty:
          | t-blank => [eq-con(T.t-id(id.id), T.t-pointer(T.t-var(id.id)))]
          | else => [eq-con(T.t-id(id.id), T.t-pointer(id.ty)),
                     sub-con(T.t-var(id.id), T.t-pointer(id.ty))]
        end
    | h-id(id) => cg-bind(id)
    | h-unbox(id) => 
        cases (T.Type) id.ty: 
          | t-blank => [eq-con(T.t-pointer(T.t-id(id.id)), T.t-var(id.id))]
          | else => [eq-con(T.t-pointer(T.t-id(id.id)), id.ty),
                     sub-con(T.t-pointer(T.t-var(id.id)), id.ty)]
        end
    | h-lam(f, closure) => cg-bind(f) + cg-bind(closure)
    | h-app(f, args) => 
        cg-bind(f) + H.flatten(args.map(cg-bind))
          + [sub-con(T.t-id(f.id), 
                     T.t-arrow(for map(a from args): T.t-id(a.id) end,
                               T.t-lettable(lettable)))]
          # TODO need to incorporate the function return type or body here
    | h-obj(fields) => 
        H.flatten(for map(f from fields): cg-bind(f.value) end)
         + [eq-con(T.t-lettable(lettable),
                   T.t-record(for map(f from fields):
                                T.t-field(f.name.name,
                                          cases (T.Type) f.value.ty:
                                            | t-blank => T.t-var(f.value.id)
                                            | else => f.value.ty
                                          end)
                              end))]
    | h-update(super, fields) => 
        cases (T.Type) super.ty:
          | t-blank => 
             # [eq-con(T.t-id(super.id), T.t-var(super.id))]
              H.flatten(for map(f from fields): cg-bind(f.value) end)
                + for map(f from fields):
                    # type assigned to field is subtype of looked-up field
                    sub-con(T.t-id(f.value.id), 
                            T.t-lookup(T.t-id(super.id), f.name.name))
                  end
          | t-record(flds) => 
              H.flatten(for map(f from fields): cg-bind(f.value) end) +
              # TODO remove blanks from type in value of s
              for fold(s from [sub-con(T.t-id(super.id), super.ty)],
                       f from fields):
                # get type of field; error if not there
                ftype = cases (Option<T.Type>) get-field(flds, f.name.name):
                  | none => raise("Field " + f.name.name + " not present on "
                             + super.id + " with type " + super.ty.tostring())
                  | some(value) => value
                end

                cases (T.Type) ftype:
                  | t-blank => 
                      [sub-con(cases (T.Type) f.value.ty:
                                 | t-blank => T.t-id(f.value.id)
                                 | else => f.value.ty
                               end, 
                               T.t-lookup(T.t-id(super.id), f.name.name))]
                  | else => 
                      cases (T.Type) f.value.ty:
                        | t-blank => 
                            [sub-con(T.t-id(f.value.id), ftype)]
                        | else => 
                            when not (is-subtype(f.value.ty, ftype)):
                              raise("Type mismatch: " + f.value.ty.tostring()
                                       + " vs " + ftype.tostring())
                            end
                            []
                      end
                end + s
              end
          | else => raise("Error: cannot unify (message TODO)")
        end
         + [eq-con(T.t-lettable(lettable), T.t-id(super.id))]
    | h-extend(super, fields) => 
        cases (T.Type) super.ty:
          | t-blank => 
          | t-record(flds) => 
          | else => raise("Error: cannot extend a non-record")
        end

        # TODO TODO TODO TODO TODO
        # TODO TODO TODO TODO TODO
        # TODO TODO TODO TODO TODO 
        # TODO TODO TODO TODO TODO
        # TODO TODO TODO TODO TODO 
    | h-env(field) => []
    | h-dot(obj, field) =>
        cg-bind(obj) + 
          cases (T.Type) obj.ty:
            | t-blank => 
                [eq-con(T.t-lettable(lettable),
                        T.t-lookup(T.t-id(obj.id), field.name))]
            | t-record(fields) => 
                cases (Option<T.Type>) get-field(fields, field):
                  | none => raise("Error: record has no field " + field)
                  | some(s) => [eq-con(T.t-lettable(lettable), s)]
                end
            | else => raise("Type mismatch: expected record and got " 
                               + obj.ty.tostring())
          end
    | h-colon(obj, field) =>
        cg-bind(obj) + 
          cases (T.Type) obj.ty:
            | t-blank => 
                [eq-con(T.t-lettable(lettable),
                        T.t-lookup(T.t-id(obj.id), field.name))]
            | t-record(fields) => 
                cases (Option<T.Type>) get-field(fields, field):
                  | none => raise("Error: record has no field " + field)
                  | some(s) => [eq-con(T.t-lettable(lettable), s)]
                end
            | else => raise("Type mismatch: expected record and got " 
                               + obj.ty.tostring())
          end
    | h-get-bang(obj, field) => 
        cg-bind(obj) + 
          cases (T.Type) obj.ty:
            | t-blank => 
                [eq-con(T.t-lettable(lettable),
                        T.t-lookup(T.t-id(obj.id), field.name))]
            | t-record(fields) => 
                cases (Option<T.Type>) get-field(fields, field):
                  | none => raise("Error: record has no field " + field)
                  | some(s) => [eq-con(T.t-lettable(lettable), s)]
                end
            | else => raise("Type mismatch: expected record and got " 
                               + obj.ty.tostring())
          end
  end
end


fun get-func(funcs :: List<AH.NamedFunc>, name :: String) -> AH.NamedFunc:
  funcs.find(fun(func :: AH.NamedFunc): func.name == name end)
end


fun cg-funcs(datas :: List<AH.NamedData>,
             funcs :: List<AH.NamedFunc>) -> List<Constraint>:
  for fold(s from [], func from funcs):
    cases (AH.NamedFunc) func: 
      | named-func(name, args, body, ret, is-closure) => 
          cg-expr(body, datas, funcs) 
           + cases (T.Type) ret:
               | t-blank => 
                   [eq-con(T.t-id(name),
                           T.t-arrow(for map(a from args):
                                       cases (T.Type) a.ty:
                                         | t-blank => T.t-var(a.id)
                                         | else => a.ty
                                       end
                                     end,
                                     T.t-expr(body)))]
               | else => 
                   [sub-con(T.t-expr(body), ret),
                    eq-con(T.t-id(name), 
                           T.t-arrow(for map(a from args):
                                       cases (T.Type) a.ty:
                                         | t-blank => T.t-var(a.id)
                                         | else => a.ty
                                       end
                                     end,
                                     ret))]
             end
           + args.map(cg-bind)
    end + s
  end
end


fun cg-datas(datas :: List<AH.NamedData>) -> List<Constraint>:
  for fold(s from [], dat from datas):
    s # TODO add actual stuff.  
  end
end


fun get-sub-cons(cons :: List<Constraint>) -> List<Constraint>:
  cases (List<Constraint>) cons: 
    | empty => empty
    | link(f, r) => if is-sub-con(f): [f] else: [] end + get-sub-cons(r) 
  end
end


fun get-eq-cons(cons :: List<Constraint>) -> List<Constraint>:
  cases (List<Constraint>) cons: 
    | empty => empty
    | link(f, r) => if is-eq-con(f): [f] else: [] end + get-eq-cons(r) 
  end
end


fun occurs(lhs :: T.Type, rhs :: T.Type) -> Boolean:
#print("IN OCCURS: " + lhs.tostring() + ", " + rhs.tostring())
  if lhs == rhs:
    true
  else:
    cases (T.Type) rhs:
      | t-pointer(val) => occurs(lhs, val)
      | t-arrow(args, ret) => 
          for fold(s from occurs(lhs, ret), a from args):
            s or occurs(lhs, a)
          end
      | t-method(args, ret) => 
          for fold(s from occurs(lhs, ret), a from args):
            s or occurs(lhs, a)
          end
      | t-record(fields) => 
          for fold(s from false, f from fields): 
            s or occurs(lhs, f.type) 
          end
      | t-lookup(obj, field) => occurs(lhs, obj)
      | else => false
    end
  end
end


fun extend-replace(l :: T.Type, 
                   r :: T.Type, 
                   subs :: List<Substitution>) -> List<Substitution>:
  fun replace(t :: T.Type, with :: T.Type, inside :: T.Type) -> T.Type:
    if t == inside:
      with
    else:
      cases (T.Type) inside:
        | t-pointer(val) => T.t-pointer(replace(l, r, val))
        | t-arrow(args, ret) => 
            # WARNING make sure this map expression truly works...
            T.t-arrow(args.map(replace(l, r, _)), replace(l, r, ret))
        | t-method(args, ret) => 
            T.t-method(args.map(replace(l, r, _)), replace(l, r, ret))
        | t-record(fields) => 
            T.t-record(for map(f from fields):
                         T.t-field(f.name, replace(l, r, f.type))
                       end)
        | t-lookup(obj, field) =>
            # WARNING this needs to be tested. I'm still a bit unsure. 
       #     new-obj = replace(l, r, obj)
       #     cases (Term) new-obj: 
       #       | t-box(_) => raise("t-box in dot expression")
       #       | t-arrow(_, _) => raise("Function in dot expression")
       #       | t-record(fields2) => 
       #           cases (Option<Term>) get-field(fields2, field):
       #             | none => raise("Field " + field + " not found!")
       #             | some(s) => s
       #           end
       #       | t-ty(type) => raise("t-ty in t-lookup case of replace()?")
       #       | else => t-lookup(new-obj, field)
       #     end
            T.t-lookup(replace(l, r, obj), field)
        | else => inside
      end
    end
  end

  when occurs(l, r):
    raise("Occurs check failed: " + l.tostring() + ", " + r.tostring())
  end

  fun replace-all(old :: List<Substitution>,
                  left :: T.Type, 
                  right :: T.Type) -> List<Substitution>:
    cases (List<Substitution>) old:
      | empty => empty
      | link(f, re) => 
          new-rhs = replace(left, right, f.rhs)
          if f.lhs == new-rhs:
            replace-all(re, left, right)
          else:
            link(let-sub(f.lhs, new-rhs), replace-all(re, left, right))
          end
    end
  end

  if l <> r:
    new-subs = link(let-sub(l, r), replace-all(subs, l, r))
    cases (Option<Term>) lookup(r, new-subs):
      | none => new-subs
      | some(s) => 
          if s <> r: extend-replace(r, s, new-subs) else: new-subs end
    end
  else:
    subs
  end
  
#  cases (Option<Term>) lookup(r, new-subs):
#    | none => new-subs
#    | some(s) => 
#        if s <> r: extend-replace(r, s, new-subs) else: new-subs end
#  end
end


fun lookup(t :: T.Type, subs :: List<Substitution>) -> Option<T.Type>:
  cases (List<Substitution>) subs:
    | empty => none
    | link(f, r) => if f.lhs == t: some(f.rhs) else: lookup(t, r) end
  end
end


#fun resolve-t-lookup(obj :: Term, 
#                     field :: String,
#                     subs :: List<Substitution>) -> Option<Term>:
#  cases (Option<Term>) lookup(obj, subs):
#    | none => none
#    | some(s) => 
#        cases (Term) s:
#          | t-record(fields) => 
#              cases (Option<Term>) get-field(fields, field):
#                | none => raise("Field not present: " + field)
#                | some(value) => some(value)
#              end
#          | t-arrow(_, _) => raise("Arrow not permitted in lookup!")
#          | t-box(_) => raise("Box not permitted in lookup!")
#          | t-lookup(obj2, field2) => 
#              cases (Option<Term>) resolve-t-lookup(obj2, field, subs):
#                | none => none
#                | some(value) => 
#                    cases (Term) value:
#                      | t-record(fields) => 
 #                         cases (Option<Term>) get-field(fields, field):
#                            | none => raise("Field not present: " + field)
#                            | some(v) => some(v)
#                          end
#                      | else => raise("Problem!")
#                    end
#              end
#          | t-ty(val) => 
#              cases (T.Type) val:
#                | t-record(fields) => 
#                    resolve-t-lookup(type-to-term(val), field, subs)
#                | else => raise("Invalid type in lookup: " + s.tostring())
#              end
#          | else => resolve-t-lookup(s, field, subs)
#        end
#  end
#end


# Unification Code
fun unify-subs(cons :: List<Constraint>, 
               subs :: List<Substitution>) -> List<Substitution>:
#print("IN UNIFY")
  if is-empty(cons):
    subs
  else if not is-eq-con(cons.first):
    print(cons.first)
    raise("unify-subs needs to be called on a set of equality constraints")
  else if cons.first.lhs == cons.first.rhs:
#print("IN UNIFY; cons are equal: " + cons.first.lhs.tostring())

    unify-subs(cons.rest, subs)
  else:
    l = cons.first.lhs
    r = cons.first.rhs

#print("IN UNIFY: " + l.tostring() + ", " + r.tostring() + ", cons size = "
 #       + cons.length().tostring())

    fun cant-unify():
      raise("Cannot unify " + l.tostring() + " with " + r.tostring())
    end

    cases (T.Type) l:
      | t-pointer(val) => 
          cases (T.Type) r:
            | t-pointer(val2) => 
                unify-subs(link(eq-con(val, val2), cons.rest), subs)
            | t-lookup(obj, field) => 
                unify-subs(link(eq-con(r, l), cons.rest), subs)
                # TODO TODO TODO 
              #  vvar = t-var(gensym("vv.v"))
              #  unify-subs([sub-con(vvar, l), sub-con(vvar, r)] + cons.rest,
              #             subs)
            # TODO this might need to be a swap case if there is a variable
            # on the right hand side of the constraint. 
            | else => cant-unify()
          end
      | t-arrow(args, ret) => 
          cases (T.Type) r:
            | t-arrow(args2, ret2) =>
                new-cons = [eq-con(ret, ret2)]
                  + for map2(a from args, a2 from args2):
                      eq-con(a, a2)
                    end + cons.rest
                unify-subs(new-cons, subs)
            | t-var(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-id(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-lettable(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-expr(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | else => raise("Function and non-function: " + l.tostring()
                              + ", " + r.tostring())
          end
      | t-record(fields) => 
          cases (T.Type) r:
            | t-record(fields2) => 
                unify-subs(map2(eq-con, fields, fields2) + cons.rest, subs)
            | else => raise("Record and non-record")
          end
      # TODO is this correct? 
      | t-lookup(obj, field) => 
   #       cases (Option<Term>) lookup(obj, subs):
   #         | none => 
   #         | some(s) => 
   #       end
          cases (T.Type) r:
            | t-lookup(obj2, field2) => 
            #    vvar = t-var(gensym("vv.v"))
             #   new-cons = [sub-con(vvar, l)] + cons.rest + [sub-con(vvar, r)]
                # For now, just get rid of pair. 
                unify-subs(cons.rest, extend-replace(l, r, subs))
            | t-var(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-id(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-lettable(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-expr(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
   #         | t-pointer(val) => 
   #             cases (Option<T.Type>) lookup(l, subs):
   #               | some(thing) => 
   #                   unify-subs(link(eq-con(thing, r), cons.rest), subs)
   #               | none => 
   #                   unify-subs(cons.rest, extend-replace(l, r, subs))
   #             end
            | else => cant-unify()
            # TODO check that last one. May be bad. 
          end
      | t-var(_) => 
          cases (Option<T.Type>) lookup(l, subs):
            | some(thing) => 
                unify-subs(link(eq-con(thing, r), cons.rest), subs)
            | none => 
                unify-subs(cons.rest, extend-replace(l, r, subs))
          end
      | t-id(_) => 
          cases (Option<T.Type>) lookup(l, subs):
            | some(thing) => 
                unify-subs(link(eq-con(thing, r), cons.rest), subs)
            | none => 
                unify-subs(cons.rest, extend-replace(l, r, subs))
          end
      | t-lettable(_) => 
          cases (Option<T.Type>) lookup(l, subs):
            | some(thing) => 
                unify-subs(link(eq-con(thing, r), cons.rest), subs)
            | none => 
                unify-subs(cons.rest, extend-replace(l, r, subs))
          end
      | t-expr(_) => 
          cases (Option<T.Type>) lookup(l, subs):
            | some(thing) => 
                unify-subs(link(eq-con(thing, r), cons.rest), subs)
            | none => 
                unify-subs(cons.rest, extend-replace(l, r, subs))
          end
      | else =>
          cases (T.Type) r:
            | t-var(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-id(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-lettable(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-expr(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | t-lookup(_,_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
            | else => 
                when l <> r:
                  raise("Type mismatch (else case): " + l.tostring() + " vs "
                           + r.tostring())
                end
                unify-subs(cons.rest, subs)
          end
    end
  end
end


#fun type-term(t :: Term, subs :: List<Substitution>) -> T.Type:
#  cases (Option<Term>) lookup(t, subs):
#    | none => T.t-any 
#    | some(s) => 
#        cases (Term) s:
#          | t-ty(val) => 
 #             cases (T.Type) val:
 #               | t-blank => 
 #                  # print("This term is blank: " + t.tostring())
 #                   T.t-any
 #               | else => val
 #             end
 #         | t-box(val) => T.t-pointer(type-term(val, subs)) 
 #         | t-arrow(args, ret) => 
 #             T.t-arrow(map(type-term(_, subs), args), type-term(ret, subs))
 #         | t-record(fields) => 
 #             T.t-record(for map(f from fields): 
 #                          T.t-field(f.name, type-term(f.type, subs)) 
 #                        end)
 #         | t-lookup(obj, field) =>
 #             cases (T.Type) type-term(obj, subs):
 #               | t-record(fields) => 
 #                   cases (Option<T.Type>) get-type-field(fields, field):
 #                     | none => raise("t-lookup in type-term: " + s.tostring() 
 #                                       + ", from " + t.tostring())
 #                     | some(ty) => ty
 #                   end
 #               | else => raise("t-lookup in type-term: " + s.tostring() 
 #                                 + ", from " + t.tostring())
 #             end
#
#              
#          | else => type-term(s, subs)
#          # WARNING make sure this doesn't infinite loop...
#        end
#  end
#end


fun check-lookup(t :: T.Type, subs :: List<Substitution>) -> T.Type:
  cases (Option<T.Type>) lookup(t, subs):
    | none => T.t-any
    | some(s) => validify-type(s)
  end
end


fun validify-type(t :: T.Type) -> T.Type:
  cases (T.Type) t:
    | t-blank => raise("ERROR")
    | t-arrow(args, ret) => 
        T.t-arrow(args.map(validify-type), validify-type(ret))
    | t-method(args, ret) => 
        T.t-method(args.map(validify-type), validify-type(ret))
    | t-record(fields) => 
        T.t-record(for map(f from fields):
                     T.t-field(f.name, validify-type(f.type))
                   end)
    | t-pointer(ty) => T.t-pointer(validify-type(ty))
    | t-var(_) => 
        when debug:
          print("Somehow this got through: " + t.tostring())
        end
        T.t-any
    | t-id(_) => 
        when debug:
          print("Somehow this got through: " + t.tostring())
        end
        T.t-any
    | t-lettable(_) => 
        when debug:
          print("Somehow this got through: " + t.tostring())
        end
        T.t-any
    | t-expr(_) => 
        when debug:
          print("Somehow this got through: " + t.tostring())
        end
        T.t-any
    | t-lookup(_, _) => 
        when debug:
          print("Somehow this got through: " + t.tostring())
        end
        T.t-any
    | else => t
  end
end


fun type-expr(expr :: AH.HExpr, subs :: List<Substitution>) -> T.Type:
  check-lookup(T.t-expr(expr), subs)
end


fun type-lettable(lettable :: AH.HLettable,
                  subs :: List<Substitution>) -> T.Type:
  check-lookup(T.t-lettable(lettable), subs)
end


fun type-id(id :: String, subs :: List<Substitution>) -> T.Type:
  check-lookup(T.t-id(id), subs)
end


# TODO the actual type assigner, which won't be that hard. 
# Once we have a complete substitution, we walk through the program and
# replace all blank T.Types with their proper values.
fun assign-expr(expr :: AH.HExpr, subs :: List<Substitution>) -> AH.HExpr:
  cases (AH.HExpr) expr:
    | h-ret(id) => AH.h-ret(id.retype-if-blank(type-id(id.id, subs)))
    | h-let(bind, val, body) => 
        AH.h-let(bind.retype-if-blank(type-lettable(val, subs)),
                 assign-lettable(val, subs),
                 assign-expr(body, subs))
    | h-assign(bind, val, body) => 
        AH.h-assign(bind.retype-if-blank(type-id(bind.id, subs)),
                    val.retype-if-blank(type-id(val.id, subs)), 
                    assign-expr(body, subs))
    | h-try(body, bind, _except) => 
        AH.h-try(assign-expr(body, subs), 
                 body.retype-if-blank(type-id(bind.id, subs)),
                 assign-expr(_except, subs))
    | h-if(c, t, e) => 
        AH.h-if(c.retype-if-blank(type-id(c.id, subs)),
                assign-expr(t, subs),
                assign-expr(e, subs))
    | h-cases(type, val, branches, _else) => 
        AH.h-cases(type, 
                   val.retype-if-blank(type-id(val.id, subs)),
                   for map(b from branches):
                     cases (AH.HCasesBranch) b:
                       | h-cases-branch(name, args, body) =>
                           new-args = for map(a from args):
                             a.retype-if-blank(type-id(a.id, subs))
                           end
                           AH.h-cases-branch(name, 
                                             new-args, 
                                             assign-expr(body, subs))
                     end
                   end,
                   cases (Option<AH.HExpr>) _else:
                     | none => none
                     | some(s) => some(assign-expr(s, subs))
                   end)
  end
end

fun assign-lettable(lettable :: AH.HLettable,
                    subs :: List<Substitution>) -> AH.HLettable:
  cases (AH.HLettable) lettable: 
    | h-undefined => AH.h-undefined
    | h-box(id) => AH.h-box(id.retype-if-blank(type-id(id.id, subs)))
    | h-id(id) => AH.h-id(id.retype-if-blank(type-id(id.id, subs)))
    | h-unbox(id) => AH.h-unbox(id.retype-if-blank(type-id(id.id, subs)))
    | h-lam(f, closure) => 
        AH.h-lam(f.retype-if-blank(type-id(f.id, subs)), 
                 closure.retype-if-blank(type-id(closure.id, subs)))
    | h-app(f, args) => 
        AH.h-app(f.retype-if-blank(type-id(f.id, subs)),
                 for map(a from args): 
                   a.retype-if-blank(type-id(a.id, subs))
                 end)
    | h-obj(fields) => AH.h-obj(map(assign-field(_, subs), fields))
    | h-update(super, fields) => 
        AH.h-update(super.retype-if-blank(type-id(super.id, subs)), 
                    map(assign-field(_, subs), fields))
    | h-extend(super, fields) =>
        AH.h-update(super.retype-if-blank(type-id(super.id, subs)),
                    map(assign-field(_, subs), fields))
    | h-env(field) => AH.h-env(field)
    | h-dot(obj, field) => 
        AH.h-dot(obj.retype-if-blank(type-id(obj.id, subs)), field)
    | h-colon(obj, field) => 
        AH.h-colon(obj.retype-if-blank(type-id(obj.id, subs)), field)
    | h-get-bang(obj, field) => 
        AH.h-get-bang(obj.retype-if-blank(type-id(obj.id, subs)), field)
  end
end

fun assign-field(field :: AH.HField, subs :: List<Substitution>) -> AH.HField:
  cases (AH.HField) field:
    | h-field(name, value) => 
        AH.h-field(name, value.retype-if-blank(type-id(value.id, subs)))
  end
end

fun assign-func(func :: AH.NamedFunc, 
                subs :: List<Substitution>) -> AH.NamedFunc:
  cases (AH.NamedFunc) func: 
    | named-func(name, args, body, ret, is-closure) => 
        AH.named-func(name, 
                      for map(a from args): 
                        a.retype-if-blank(type-id(a.id, subs)) 
                      end,
                      assign-expr(body, subs),
                      type-expr(body, subs),
                      is-closure)
  end
end



fun infer-prog(program):
  datas = program.datas
  funcs = program.funcs
  globals = program.globals
  expr = program.expr

  cons = cg-expr(expr, datas, funcs) + cg-funcs(datas, funcs) + cg-datas(datas)

  subs = unify-subs(get-eq-cons(cons), empty)

  new-expr = assign-expr(expr, subs)
  new-funcs = map(assign-func(_, subs), funcs)

  # TODO datas
  new-datas = datas
  # TODO globals? 
  new-globals = globals

  {globals : new-globals,
   datas : new-datas,
   funcs : new-funcs,
   expr : new-expr}
end
