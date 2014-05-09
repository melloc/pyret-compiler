#lang pyret

provide *

import "types.arr" as T
import "ast-common.arr" as AC
import "helpers.arr" as H
import "ast-h.arr" as AH


data Constraint:
  | eq-con(lhs :: Term, rhs :: Term)
end

data Term:
  | t-expr(val :: AH.HExpr)
  | t-lettable(val :: AH.HLettable)
  | t-id(val :: String)
  | t-var(val :: String)
  | t-ty(val :: T.Type)
  | t-box(val :: Term)
  | t-arrow(args :: List<Term>, ret :: Term)
  | t-record(fields :: List<TermField>)
  | t-lookup(obj :: Term, field :: String)
sharing:
  tostring(self):
    cases (Term) self:
      | t-expr(val) => "expr"
      | t-lettable(val) => "lettable"
      | t-id(val) => "id(" + val.tostring() + ")"
      | t-var(val) => "var(" + val.tostring() + ")"
      | t-ty(val) => "ty(" + val.tostring() + ")"
      | t-box(val) => "box(" + val.tostring() + ")"
      | t-arrow(args, ret) => 
          "(" + args.join-str(",") + ") -> " + ret.tostring()
      | t-record(fields) => "{" + fields.join-str(",") + "}"
      | t-lookup(obj, field) => obj.tostring() + "." + field
    end
  end
end

data TermField:
  | t-field(name :: String, type :: Term)
end

data Substitution:
  | let-sub(lhs :: Term, rhs :: Term)
end


fun type-to-term(t :: T.Type) -> Term:
  cases (T.Type) t:
    | t-arrow(args, ret) => 
        t-arrow(args.map(type-to-term), type-to-term(ret))
    | t-method(args, ret) => 
        t-arrow(args.map(type-to-term), type-to-term(ret))
    | t-record(fields) => 
        t-record(for map(f from fields): 
                   t-field(f.name, type-to-term(f.type)) 
                 end)
    | t-pointer(ty) => t-box(type-to-term(ty))
    | else => t-ty(t)
  end
end


fun typecheck-failed(l :: Term, r :: Term):
  raise("Typecheck failed: expected" + l.tostring() + "; got " + r.tostring())
end


fun cg-expr(expr :: AH.HExpr, 
            datas :: List<AH.NamedData>,
            funcs :: List<AH.NamedFunc>) -> List<Constraint>:
  cases (AH.HExpr) expr:
    | h-ret(id) => 
        cases (T.Type) id.ty:
          | t-blank => [eq-con(t-id(id.id), t-var(id.id))]
          | else => [eq-con(t-id(id.id), t-ty(id.ty))]
        end
    | h-let(bind, val, body) => 
        cases (T.Type) bind.ty:
          | t-blank => [eq-con(t-var(bind.id), t-lettable(val))]
          | else => [eq-con(t-var(bind.id), t-ty(bind.ty)),
                     eq-con(t-lettable(val), t-ty(bind.ty))]
        end
          + cg-expr(body, datas, funcs) + cg-lettable(val, funcs) 
          + [eq-con(t-expr(expr), t-expr(body))]
    | h-assign(bind, val, body) =>
        cases (T.Type) bind.ty:
          | t-blank => 
              cases (T.Type) val.ty:
                | t-blank => [eq-con(t-id(val.id), t-var(val.id)),
                              eq-con(t-var(bind.id), t-var(val.id))]
                | else => [eq-con(t-id(bind.id), t-ty(val.ty))]
              end
          | t-pointer(ty) => 
              cases (T.Type) val.ty:
                | t-blank => [eq-con(t-id(val.id), t-ty(ty))]
                | else => 
                    when not (ty == val.ty):
                      raise("Typecheck failed: " + ty.tostring()
                              + " vs " + val.ty.tostring())
                    end
                    []
              end
          | else => raise("Assignment can only be done to a pointer")
        end
          + cg-expr(body, datas, funcs) + [eq-con(t-expr(expr), t-expr(body))]
    | h-try(body, bind, _except) => 
        cases (T.Type) bind.ty:
          | t-blank => [eq-con(t-var(bind.id), t-ty(T.t-any))]
                       # TODO not sure what to do here. 
                       # We *could* look through and find the raise type
                       # as well as the return type, but that would be hard. 
          | else => [eq-con(t-var(bind.id), t-ty(bind.ty))]
        end
          + cg-expr(body, datas, funcs) + cg-expr(_except, datas, funcs)
          + [eq-con(t-expr(expr), t-expr(body)),
             eq-con(t-expr(expr), t-expr(_except)),
             eq-con(t-expr(body), t-expr(_except))]
    | h-if(c, t, e) => 
        cases (T.Type) c.ty:
          | t-blank => [eq-con(t-id(c.id), t-var(c.id))]
          | else => [eq-con(t-id(c.id), t-ty(c.ty))]
        end
          + cg-expr(t, datas, funcs) + cg-expr(e, datas, funcs) 
          + [eq-con(t-expr(expr), t-expr(t)),
             eq-con(t-expr(expr), t-expr(e)),
             eq-con(t-expr(t), t-expr(e))] # is this an infinite loop?
    | h-cases(type, val, branches, _else) => 
        when not T.is-t-name(type):
          raise("cases only works with named types. ")
        end
        
        opdat = datas.find(fun(nd :: AH.NamedData) -> Bool: 
                             nd.name == type.name 
                           end)

        when is-none(opdat):
          raise("Not a valid type for cases statement")
        end

        dat = opdat.value

        cases (T.Type) val.ty:
          | t-blank => [eq-con(t-id(val.id), t-ty(type))]
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
                            | t-blank => eq-con(t-id(a.id), t-ty(m.bind.ty))
                            | else =>
                                when not (a.ty == m.bind.ty):
                                  raise("Type mismatch in cases binding")
                                end
                                eq-con(t-id(a.id), t-ty(a.ty))
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
                   + [eq-con(t-expr(body), t-expr(expr))]
             end
           end
         + cases (Option<AH.HExpr>) _else:
             | none => []
             | some(s) => 
                 cg-expr(s, datas, funcs) + [eq-con(t-expr(s), t-expr(expr))]
           end 
  end
end


fun get-field(rec :: List<TermField>, name :: String) -> Option<Term>:
  cases (List<TermField>) rec: 
    | empty => none
    | link(f, r) => 
        if f.name == name: some(f.type) else: get-field(r, name) end
  end
end


fun get-type-field(rec :: List<T.TypeField>, 
                   name :: String) -> Option<T.Type>:
  cases (List<T.TypeField>) rec:
    | empty => none
    | link(f, r) => 
        if f.name == name: some(f.type) else: get-type-field(r, name) end
  end
end


fun cg-lettable(lettable :: AH.HLettable, 
                funcs :: List<AH.NamedFunc>) -> List<Constraint>:
  cases (AH.HLettable) lettable:
    | h-undefined => [] # I think this is right...
    | h-box(id) =>
        cases (T.Type) id.ty:
          | t-blank => [eq-con(t-id(id.id), t-box(t-var(id.id)))]
          | else => [eq-con(t-id(id.id), t-box(t-ty(id.ty)))]
        end
    | h-id(id) => 
        cases (T.Type) id.ty:
          | t-blank => [eq-con(t-id(id.id), t-var(id.id))]
          | else => [eq-con(t-id(id.id), t-ty(id.ty))]
        end
    | h-unbox(id) => 
        cases (T.Type) id.ty: 
          | t-blank => [eq-con(t-box(t-id(id.id)), t-var(id.id))]
          | else => [eq-con(t-box(t-id(id.id)), t-ty(id.ty))]
        end
    | h-lam(f, closure) => 
        cases (T.Type) f.ty:
           | t-blank => [eq-con(t-id(f.id), t-var(f.id))]
           | else => [eq-con(t-id(f.id), t-ty(f.ty))]
        end
         + cases (T.Type) closure.ty:
             | t-blank => [eq-con(t-id(closure.id), t-var(closure.id))]
             | else => [eq-con(t-id(closure.id), t-ty(closure.ty))]
           end
    | h-app(f, args) => 
        cases (T.Type) f.ty:
          | t-blank => [eq-con(t-id(f.id), t-var(f.id))]
          | else => [eq-con(t-id(f.id), t-ty(f.ty))]
        end
          + for map(a from args):
              cases (T.Type) a.ty:
                | t-blank => eq-con(t-id(a.id), t-var(a.id))
                | else => eq-con(t-id(a.id), t-ty(a.ty))
              end
            end
          + [eq-con(t-id(f.id), t-arrow(for map(a from args):
                                          t-id(a.id)
                                        end,
                                        t-lettable(lettable)))]
    | h-obj(fields) => 
        for map(f from fields):
          cases (T.Type) f.value.ty:
            | t-blank => eq-con(t-id(f.value.id), t-var(f.value.id))
            | else => eq-con(t-id(f.value.id), t-ty(f.value.ty))
          end
        end
         + [eq-con(t-lettable(lettable),
                   t-record(for map(f from fields):
                              cases (T.Type) f.value.ty:
                                | t-blank => 
                                    t-field(f.name.name, t-var(f.value.id))
                                | else => 
                                    t-field(f.name.name, t-ty(f.value.ty))
                              end
                            end))]
    | h-update(super, fields) => 
        cases (T.Type) super.ty:
          | t-blank => 
              for fold(s from [eq-con(t-id(super.id), t-var(super.id))],
                       f from fields):
                [cases (T.Type) f.value.ty:
                   | t-blank => eq-con(t-id(f.value.id), t-var(f.value.id))
                   | else => eq-con(t-id(f.value.id), t-ty(f.value.ty))
                 end,
                 eq-con(t-box(t-id(f.value.id)), 
                        t-lookup(t-id(super.id), f.name.name))] + s
              end
          | t-record(fields2) => 
              for fold(s from [eq-con(t-id(super.id), t-ty(super.ty))],
                       f from fields):
                # TODO get type of field; error if not there
                ftype = cases (Option<T.Type>) get-type-field(fields2,
                                                              f.name.name):
                  | none => raise("Field " + f.name.name + " not present")
                  | some(value) => value
                end

                cases (T.Type) f.value.ty:
                   | t-blank => [eq-con(t-id(f.value.id), t-ty(ftype))]
                   | else => 
                       when not (f.value.ty == ftype):
                         raise("Type mismatch: " + f.value.ty.tostring()
                                + " vs " + ftype.tostring())
                       end
                       [eq-con(t-id(f.value.id), t-ty(f.value.ty))]
                end + s
              end
          | else => raise("Error: cannot unify (message TODO)")
        end
         + [eq-con(t-lettable(lettable), t-var(super.id))]
    | h-extend(super, fields) => # TODO
    | h-env(field) => []
    | h-dot(obj, field) => 
        cases (T.Type) obj.ty:
          | t-blank => 
              [eq-con(t-lettable(lettable), 
                      t-lookup(t-var(obj.id), field.name))]
          | t-record(fields) => 
              cases (Option<T.Type>) get-type-field(fields, field):
                | none => raise("Error: record has no field " + field)
                | some(s) => [eq-con(t-lettable(lettable), t-ty(s))]
              end
          # TODO handle dot notation with ADTs
          | else => raise("Error: cannot unify (message TODO)")
        end
    | h-colon(obj, field) =>
        cases (T.Type) obj.ty:
          | t-blank => 
              [eq-con(t-lettable(lettable), 
                      t-lookup(t-var(obj.id), field.name))]
          | t-record(fields) => 
              cases (Option<T.Type>) get-type-field(fields, field):
                | none => raise("Error: record has no field " + field)
                | some(s) => [eq-con(t-lettable(lettable), t-ty(s))]
              end 
          | else => raise("Error: cannot unify (message TODO)")
        end
    | h-get-bang(obj, field) => 
        cases (T.Type) obj.ty:
          | t-blank => 
              [eq-con(t-lettable(lettable), 
                      t-lookup(t-var(obj.id), field.name))]
          | t-record(fields) => 
              cases (Option<T.Type>) get-type-field(fields, field.name):
                | none => raise("Error: record has no field " + field)
                | some(s) => [eq-con(t-lettable(lettable), t-ty(s))]
              end
          | else => raise("Error: cannot unify (message TODO)")
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
           + [eq-con(t-expr(body), t-ty(ret)),
              eq-con(t-id(name), t-arrow(for map(a from args):
                                           cases (T.Type) a.ty:
                                             | t-blank => t-var(a.id)
                                             | else => t-ty(a.ty)
                                           end
                                         end,
                                         t-ty(ret)))]
           + for map(a from args):
               cases (T.Type) a.ty:
                 | t-blank => eq-con(t-id(a.id), t-var(a.id))
                 | else => eq-con(t-id(a.id), t-ty(a.ty))
               end
             end
    end + s
  end
end


fun cg-datas(datas :: List<AH.NamedData>) -> List<Constraint>:
  for fold(s from [], dat from datas):
    s # TODO add actual stuff.  
  end
end


fun occurs(lhs :: Term, rhs :: Term) -> Boolean:
#print("IN OCCURS: " + lhs.tostring() + ", " + rhs.tostring())
  if lhs == rhs:
    true
  else:
    cases (Term) rhs:
      | t-box(val) => occurs(lhs, val)
      | t-arrow(args, ret) => 
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


fun extend-replace(l :: Term, 
                   r :: Term, 
                   subs :: List<Substitution>) -> List<Substitution>:
  fun replace(t :: Term, with :: Term, inside :: Term) -> Term:
    if t == inside:
      with
    else:
      cases (Term) inside:
        | t-box(val) => t-box(replace(l, r, val))
        | t-arrow(args, ret) => 
            # WARNING make sure this map expression truly works...
            t-arrow(map(replace(l, r, _), args), replace(l, r, ret))
        | t-record(fields) => 
            t-record(for map(f from fields):
                       t-field(f.name, replace(l, r, f.type))
                     end)
        | t-lookup(obj, field) =>
            # WARNING this needs to be tested. I'm still a bit unsure. 
            new-obj = replace(l, r, obj)
            cases (Term) new-obj: 
              | t-box(_) => raise("t-box in dot expression")
              | t-arrow(_, _) => raise("Function in dot expression")
              | t-record(fields2) => 
                  cases (Option<Term>) get-field(fields2, field):
                    | none => raise("Field " + field + " not found!")
                    | some(s) => s
                  end
              | t-ty(type) => raise("t-ty in t-lookup case of replace()?")
              | else => t-lookup(new-obj, field)
            end
        | else => inside
      end
    end
  end

  when occurs(l, r):
    raise("Occurs check failed: " + l.tostring() + ", " + r.tostring())
  end

  fun replace-all(old :: List<Substitution>,
                  left :: Term, 
                  right :: Term) -> List<Substitution>:
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


fun lookup(t :: Term, subs :: List<Substitution>) -> Option<Term>:
  cases (List<Substitution>) subs:
    | empty => none
    | link(f, r) => if f.lhs == t: some(f.rhs) else: lookup(t, r) end
  end
end


# Unification Code
fun unify-subs(cons :: List<Constraint>, 
               subs :: List<Substitution>) -> List<Substitution>:
#print("IN UNIFY")
  if is-empty(cons):
    subs
  else if cons.first.lhs == cons.first.rhs:
#print("IN UNIFY; cons are equal: " + cons.first.lhs.tostring())

    unify-subs(cons.rest, subs)
  else:
    l = cons.first.lhs
    r = cons.first.rhs

#print("IN UNIFY: " + l.tostring() + ", " + r.tostring())

    fun cant-unify():
      raise("Cannot unify " + l.tostring() + " with " + r.tostring())
    end

    cases (Term) l:
      | t-ty(val) =>
          if val.is-composite():
            unify-subs(link(eq-con(type-to-term(val), r), cons.rest), subs)
          else:
            cases (Term) r:
              | t-ty(val2) => 
                  if val == val2:
                    unify-subs(cons.rest, subs)
                  else:
                    # TODO fail, or maybe check for any?
                    cant-unify()
                  end
              # TODO careful with the else case. It might need to continue
              # instead of simply failing; we shall see. There might be an
              # unsubstituted variable on the right-hand side. 
              | t-var(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
              | t-id(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
              | t-lettable(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
              | t-expr(_) => unify-subs(link(eq-con(r, l), cons.rest), subs)
              | else => cant-unify()
            end
          end
      | t-box(val) => 
          cases (Term) r:
            | t-box(val2) => 
                unify-subs(link(eq-con(val, val2), cons.rest), subs)
            | t-ty(val2) => 
                cases (T.Type) val2:
                  | t-pointer(to) => 
                      unify-subs(link(eq-con(val, t-ty(to)), cons.rest), subs)
                  | else => cant-unify()
                end
            # TODO this might need to be a swap case if there is a variable
            # on the right hand side of the constraint. 
            | else => cant-unify()
          end
      | t-arrow(args, ret) => 
          cases (Term) r:
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
            | t-ty(val) => 
                cases (T.Type) val:
                  | t-arrow(_, _) => 
                      unify-subs(link(eq-con(l, type-to-term(val)), 
                                      cons.rest), 
                                 subs)
                  | else => raise("Function and non-function")
                end
            | else => raise("Function and non-function: " + l.tostring()
                              + ", " + r.tostring())
          end
      | t-record(fields) => 
          cases (Term) r:
            | t-record(fields2) => 
                unify-subs(map2(eq-con, fields, fields2) + cons.rest, subs)
            | else => raise("Record and non-record")
          end
      # TODO is this correct? 
      | t-lookup(obj, field) => raise("Fail? Got t-lookup in unification.")
      | else =>
          # This case handles t-expr, t-lettable, t-id, and t-var, all 
          # of which are basically "variables" at this point. 
          cases (Option<Term>) lookup(l, subs):
            | some(thing) =>
          #      print("Thing: " + thing.tostring())
                unify-subs(link(eq-con(thing, r), cons.rest), subs)
            | none => 
          #      print("Nothing :(")
                unify-subs(cons.rest, extend-replace(l, r, subs))
          end
    end
  end
end


fun type-term(t :: Term, subs :: List<Substitution>) -> T.Type:
  cases (Option<Term>) lookup(t, subs):
    | none => T.t-any 
    | some(s) => 
        cases (Term) s:
          | t-ty(val) => val
          | t-box(val) => T.t-pointer(type-term(val, subs)) 
          | t-arrow(args, ret) => 
              T.t-arrow(map(type-term(_, subs), args), type-term(ret, subs))
          | t-record(fields) => 
              T.t-record(for map(f from fields): 
                           T.t-field(f.name, type-term(f.type, subs)) 
                         end)
          | t-lookup(obj, field) =>
              cases (T.Type) type-term(obj, subs):
                | t-record(fields) => 
                    cases (Option<T.Type>) get-type-field(fields, field):
                      | none => raise("t-lookup in type-term: " + s.tostring() 
                                        + ", from " + t.tostring())
                      | some(ty) => ty
                    end
                | else => raise("t-lookup in type-term: " + s.tostring() 
                                  + ", from " + t.tostring())
              end

              
          | else => type-term(s, subs)
          # WARNING make sure this doesn't infinite loop...
        end
  end
end


fun type-expr(expr :: AH.HExpr, subs :: List<Substitution>) -> T.Type:
  type-term(t-expr(expr), subs)
end


fun type-lettable(lettable :: AH.HLettable,
                  subs :: List<Substitution>) -> T.Type:
  type-term(t-lettable(lettable), subs)
end


fun type-id(id :: String, subs :: List<Substitution>) -> T.Type:
  type-term(t-id(id), subs)
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

  subs = unify-subs(cons, empty)

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
