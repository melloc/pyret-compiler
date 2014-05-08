#lang pyret

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
end

data TermField:
  | t-field(name :: String, type :: Term)
end

data Substitution:
  | let-sub(lhs :: Term, rhs :: Term)
end


fun cg-expr(expr :: AH.HExpr) -> List<Constraint>:
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
          + cg-expr(body) + cg-lettable(val) 
          + [eq-con(t-expr(expr), t-expr(body))]
    | h-assign(bind, val, body) =>
        cases (T.Type) bind.ty:
          | t-blank => [eq-con(t-var(bind.id), t-box(t-lettable(val)))]
          | t-pointer(ty) => [eq-con(t-var(bind.id), t-ty(ty)),
                              eq-con(t-lettable(val), t-ty(ty))]
          | else => raise("Assignment can only be done to a pointer")
        end
          + cg-expr(body) + cg-lettable(val)
          + [eq-con(t-expr(expr), t-expr(body))]
    | h-try(body, bind, _except) => 
        # TODO TODO TODO 
    | h-if(c, t, e) => 
        cases (T.Type) c.ty:
          | t-blank => [eq-con(t-id(c.id), t-var(c.id))]
          | else => [eq-con(t-id(c.id), t-ty(c.ty))]
        end
          + cg-expr(t) + cg-expr(e) 
          + [eq-con(t-expr(expr), t-expr(t)),
             eq-con(t-expr(expr), t-expr(e)),
             eq-con(t-expr(t), t-expr(e))] # TODO is this an infinite loop?
    | h-cases(type, val, branches, _else) => # TODO
  end
end


fun get-field(rec :: List<TermField>, name :: String) -> Option<Term>:
  cases (List<TermField>) rec: 
    | empty => none
    | link(f, r) => if f.name == name: f.type else: get-field(r, name) end
  end
end


fun cg-lettable(lettable :: AH.HLettable) -> List<Constraint>:
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
    | h-lam(f, closure) => # TODO TODO TODO this is hard...
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
        # TODO for each field, we need these constraints:
        #  - field exists and has same type (type of ID equals field lookup)
        #  - regular ID constraint
        # Additionally, we need this constraint:
        #  - type of this lettable is equal to type of super
    | h-extend(super, fields) => 
    | h-env(field) => 
    | h-dot(obj, field) => 
        cases (T.Type) obj.ty:
          | t-blank => 
              [eq-con(t-lettable(lettable), 
                      t-lookup(t-var(obj.id), field.name))]
          | t-record(fields) => 
              cases (Option<Term>) get-field(fields, field):
                | none => raise("Error: record has no field " + field)
                | some(s) => [eq-con(t-lettable(lettable), s)]
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
              cases (Option<Term>) get-field(fields, field):
                | none => raise("Error: record has no field " + field)
                | some(s) => [eq-con(t-lettable(lettable), s)]
              end 
          | else => raise("Error: cannot unify (message TODO)")
        end
    | h-get-bang(obj, field) => 
        cases (T.Type) obj.ty:
          | t-blank => 
              [eq-con(t-lettable(lettable), 
                      t-lookup(t-var(obj.id), field.name))]
          | t-record(fields) => 
              cases (Option<Term>) get-field(fields, field):
                | none => raise("Error: record has no field " + field)
                | some(s) => [eq-con(t-lettable(lettable), s)]
              end
          | else => raise("Error: cannot unify (message TODO)")
        end
  end
end


fun occurs(lhs :: Term, rhs :: Term) -> Boolean:
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
            # TODO make sure this map expression truly works...
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
                    | none => raise("Field " + field " not found!")
                    | some(s) => s
                  end
              | t-ty(type) => raise("t-ty in t-lookup case of replace()?")
              | else => t-lookup(new-obj, field)
            end
        | else => inside
      end
    end
  end

  if occurs(l, r):
    raise("Occurs check failed (message TODO)")
  else:
    link(let-sub(l, r),
         for map(s from subs): 
           let-sub(s.lhs, replace(l, r, s.rhs))
         end)
  end
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
  if is-empty(cons):
    subs
  else:
    l = cons.first.lhs
    r = cons.first.rhs
    cases (Term) subs.first.lhs:
      # TODO consolidate all of these...
      | t-expr(val) => 
          cases (Option<Term>) lookup(l, subs):
            | some(thing) => 
                unify-subs(link(eq-con(thing, r), cons.rest), subs)
            | none => 
                unify-subs(cons.rest, extend-replace(l, r, subs))
      | t-lettable(val) => 
          cases (Option<Term>) lookup(l, subs):
            | some(thing) => 
                unify-subs(link(eq-con(thing, r), cons.rest), subs)
            | none => 
                unify-subs(cons.rest, extend-replace(l, r, subs))
      | t-id(val) => 
          cases (Option<Term>) lookup(l, subs):
            | some(thing) => 
                unify-subs(link(eq-con(thing, r), cons.rest), subs)
            | none => 
                unify-subs(cons.rest, extend-replace(l, r, subs))
      | t-var(val) => 
          cases (Option<Term>) lookup(l, subs):
            | some(thing) => 
                unify-subs(link(eq-con(thing, r), cons.rest), subs)
            | none => 
                unify-subs(cons.rest, extend-replace(l, r, subs))
          end
      | t-ty(val) => 
          cases (Term) r:
            | t-ty(val2) => 
                if val == val2:
                  unify-subs(cons.rest, subs)
                else:
                  # TODO fail, or maybe check for any?
                  raise("Incompatible base types. Was one any?")
                end
            # TODO careful with the else case. It might need to continue
            # instead of simply failing; we shall see. There might be an
            # unsubstituted variable on the right-hand side. 
            | else => raise("Cannot unify two incompatible base types")
          end
      | t-box(val) => # TODO
      | t-arrow(args, ret) => 
          cases (Term) r:
            | t-arrow(args2, ret2) =>
                new-cons = [eq-con(ret, ret2)]
                  + for map2(a from args, a2 from args2):
                      eq-con(a, a2)
                    end + cons
                unify-subs(new-cons, subs)
            | else => raise("Function and non-function")
          end
      | t-record(fields) => 
          cases (Term) r:
            | t-record(fields2) => 
                unify-subs(map2(eq-con, fields, fields2) + cons, subs)
            | else => raise("Record and non-record")
          end
      | t-lookup(obj, field) => 
    end
  end
end


fun type-term(t :: Term, subs :: List<Substitution>) -> T.Type:
  cases (Option<Term>) lookup(t, subs):
    | none => # TODO just return any? Or fail for now? 
    | some(s) => 
        cases (Term) s:
          | t-expr(expr) => 
          | t-lettable(lettable) => 
          | t-id(id) => 
          | t-var(val) => 
          | t-ty(val) => val
          | t-box(val) => T.t-pointer(type-term(val, subs)) 
          | t-arrow(args, ret) => 
              T.t-arrow(map(type-term(_, subs), args), type-term(ret, subs))
          | t-record(fields) => 
              T.t-record(map(type-term(_, subs), fields))
          | t-lookup(obj, field) => 
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
    | h-ret(id) => h-ret(id.retype-if-blank(type-id(id.id, subs)))
    | h-let(bind, val, body) => 
        h-let(bind.retype-if-blank(type-lettable(val, subs)),
              assign-lettable(val, subs),
              assign-expr(body, subs))
    | h-assign(bind, val, body) => 
        h-assign(bind.retype-if-blank(type-id(bind.id, subs)),
                 assign-lettable(val, subs), 
                 assign-expr(body, subs))
    | h-try(body, bind, _except) => # TODO
    | h-if(c, t, e) => # TODO
    | h-cases(type, val, branches, _else) => # TODO
  end
end

fun assign-lettable(lettable :: AH.HLettable,
                    subs :: List<Substitution>) -> AH.HLettable:
  cases (AH.HLettable) lettable: 
    | h-undefined => h-undefined
    | h-box(id) => h-box(id.retype-if-blank(type-id(id.id, subs)))
    | h-id(id) => h-id(id.retype-if-blank(type-id(id.id, subs)))
    | h-unbox(id) => h-unbox(id.retype-if-blank(type-id(id.id, subs)))
    | h-lam(f, closure) => # TODO
    | h-app(f, args) => # TODO
    | h-obj(fields) => # TODO
    | h-update(super, fields) => # TODO
    | h-extend(super, fields) => # TODO
    | h-env(field) => # TODO
    | h-dot(obj, field) => 
        h-dot(obj.retype-if-blank(type-id(obj.id, subs)), field)
    | h-colon(obj, field) => 
        h-colon(obj.retype-if-blank(type-id(obj.id, subs)), field)
    | h-get-bang(obj, field) => 
        h-get-bang(obj.retype-if-blank(type-id(obj.id, subs)), field)
  end
end



fun infer-prog(program):
  datas = program.datas
  funcs = program.funcs
  globals = program.globals
  expr = program.expr

  # TODO the rest of this...
end
