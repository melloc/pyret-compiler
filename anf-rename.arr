#lang pyret

provide *
import "ast-anf.arr" as N
import "ast-common.arr" as AC
import "helpers.arr" as H

fun rename-value(value :: N.AVal, _from :: AC.Bind, to :: AC.Bind) -> N.AVal:
  fun maybe-rename(id :: String) -> String:
    if id == _from.id: to.id
    else: id;
  end
  cases(N.AVal) value:
    | a-id(l, id)        =>
      N.a-id(l, maybe-rename(id))
    | a-id-var(l, id)    =>
      N.a-id-var(l, maybe-rename(id))
    | a-id-letrec(l, id) =>
      N.a-id-letrec(l, maybe-rename(id))
    | else               =>
      value
  end
end

fun rename-fields(fields :: List<N.AField>, _from :: AC.Bind, to :: AC.Bind) -> List<N.AField>:
    for map(field from fields):
      cases(N.AField) field:
        | a-field(l, f, value) =>
          N.a-field(l, f, value^rename-value(_from, to))
      end
    end
  end

fun rename-variant(variant :: N.AVariant, _from :: AC.Bind, to :: AC.Bind) -> N.AVariant:
  maybe-rename-fields = rename-fields(_, _from, to)
  cases(N.AVariant) variant:
    | a-variant(l, name, members, with-members)  =>
      N.a-variant(l, name, members, maybe-rename-fields(with-members))
    | a-singleton-variant(l, name, with-members) =>
      N.a-singleton-variant(l, name, maybe-rename-fields(with-members))
  end
end

fun rename-lettable(lettable :: N.ALettable, _from :: AC.Bind, to :: AC.Bind) -> N.ALettable:
  fun maybe-rename(id :: AC.Bind) -> AC.Bind:
    if id == _from: to
    else: id;
  end
  maybe-rename-fields = rename-fields(_, _from, to)
  cases(N.ALettable) lettable:
    | a-data-expr(l, name, variants, shared) =>
      # TODO: Rename type and variant
      new-shared = maybe-rename-fields(shared)
      N.a-data-expr(l, name, variants, new-shared)
    | a-assign(l, id, value) =>
      N.a-assign(l, maybe-rename(id), rename-value(value, _from, to))
    | a-app(l, f, args) =>
      N.a-app(l, f^rename-value(_from, to), args.map(rename-value(_, _from, to)))
    | a-help-app(l, f, args) =>
      N.a-help-app(l, maybe-rename(f), args.map(rename-value(_, _from, to)))
    | a-obj(l, fields) =>
      new-fields = maybe-rename-fields(fields)
      N.a-obj(l, new-fields)
    | a-update(l, super, fields) =>
      new-super  = rename-value(super, _from, to)
      new-fields = maybe-rename-fields(fields)
      N.a-update(l, new-super, new-fields)
    | a-extend(l, super, fields) =>
      new-super  = rename-value(super, _from, to)
      new-fields = maybe-rename-fields(fields)
      N.a-extend(l, new-super, new-fields)
    | a-dot(l, obj, field) =>
      new-obj   = rename-value(obj, _from, to)
      new-field = rename-value(field, _from, to)
      N.a-dot(l, new-obj, new-field)
    | a-colon(l, obj, field) =>
      new-obj   = rename-value(obj, _from, to)
      new-field = rename-value(field, _from, to)
      N.a-colon(l, new-obj, new-field)
    | a-get-bang(l, obj, field) =>
      new-obj   = rename-value(obj, _from, to)
      new-field = rename-value(field, _from, to)
      N.a-get-bang(l, new-obj, new-field)
    | a-lam(l, args, ret, body) =>
      new-args = args.map(maybe-rename)
      new-body = rename-expr(body, _from, to)
      N.a-lam(l, new-args, ret, new-body)
    | a-method(l, args, ret, body) =>
      new-args = args.map(maybe-rename)
      new-body = rename-expr(body, _from, to)
      N.a-method(l, new-args, ret, new-body)
    | a-val(v) =>
      N.a-val(rename-value(v, _from, to))
  end
end

fun rename-expr(expr :: N.AExpr, _from :: AC.Bind, to :: AC.Bind) -> N.AExpr:
  cases(N.AExpr) expr:
    | a-let(l, bind, e, body) =>
      if _from == bind: 
        expr
      else: 
        new-e    = rename-lettable(e, _from, to)
        new-body = rename-expr(body, _from, to)
        N.a-let(l, bind, new-e, new-body)
      end
    | a-var(l, bind, e, body) =>
      if _from == bind: 
        expr
      else: 
        new-e    = rename-lettable(e, _from, to)
        new-body = rename-expr(body, _from, to)
        N.a-var(l, bind, new-e, new-body)
      end
    | a-try(l, body, b, _except) =>
      new-body   = rename-expr(body, _from, to)
      tmp-except = if _from == b:
                     _except
                   else:
                     rename-expr(_except, _from, to)
                   end
      new-b      = gensym(b + ".")
      new-except = rename-expr(tmp-except, b, new-b)
      N.a-try(l, new-body, new-b, new-except)
    | a-split-app(l, is-var, f, args, helper, helper-args) =>
      raise("a-split-app rename not supported")
    | a-if(l, c, t, e) =>
      renamed-c = c^rename-value(_from, to)
      renamed-t = t^rename-expr(_from, to)
      renamed-e = e^rename-expr(_from, to)
      N.a-if(l, renamed-c, renamed-t, renamed-e)
    | a-cases(l, type, val, branches, _else) =>
      new-val  = val^rename-value(_from, to)
      new-branches = for map(branch from branches):
        cases(N.ACasesBranch) branch:
          | a-cases-branch(l2, name, args, body) =>
            new-body = body^rename-expr(_from, to)
            N.a-cases-branch(l2, name, args, new-body)
        end
      end
      new-else = cases(Option<N.AExpr>) _else:
                   | some(e) =>
                     some(e^rename-expr(_from, to))
                   | none    =>
                     none
                 end
      N.a-cases(l, type, new-val, new-branches, new-else)
    | a-lettable(e) =>
      N.a-lettable(e^rename-lettable(_from, to))
  end

end


fun anf-rename-lettable(lettable :: N.ALettable) -> N.ALettable:
  cases(N.ALettable) lettable:
    | a-lam(l, args, ret, body) =>
      N.a-lam(l, args, ret, anf-rename-expr(body))
    | a-method(l, args, ret, body) =>
      N.a-method(l, args, ret, anf-rename-expr(body))
    | else =>
      lettable
  end
end

fun anf-rename-expr(expr :: N.AExpr) -> N.AExpr:
  cases(N.AExpr) expr:
    | a-let(l, bind, e, body) =>
      new-bind = bind.rename(gensym(bind.id + "."))
      new-e    = e^rename-lettable(bind, new-bind)^anf-rename-lettable()
      new-body = body^rename-expr(bind, new-bind)^anf-rename-expr()
      N.a-let(l, new-bind, new-e, new-body)
    | a-var(l, bind, e, body) =>
      new-bind = bind.rename(gensym(bind.id + "."))
      new-e    = e^rename-lettable(bind, new-bind)^anf-rename-lettable()
      new-body = body^rename-expr(bind, new-bind)^anf-rename-expr()
      N.a-var(l, new-bind, new-e, new-body)
    | a-try(l, body, b, _except) =>
      tmp-except = _except^anf-rename-lettable()
      new-b      = b.rename(gensym(b.id + "."))
      new-except = tmp-except^rename-expr(b, new-b)
      new-body   = body^anf-rename-expr()
      N.a-try(l, new-b, new-except, new-body)
    | a-split-app(l, is-var, f, args, helper, helper-args) =>
      raise("a-split-app not supported right now")
    | a-if(l, c, t, e) =>
      renamed-t = anf-rename-expr(t)
      renamed-e = anf-rename-expr(e)
      N.a-if(l, c, renamed-t, renamed-e)
    | a-cases(l, type, val, branches, _else) =>
      new-branches = for map(branch from branches):
        cases(N.ACasesBranch) branch:
          | a-cases-branch(l2, name, args, body) =>
            start = H.pair([], body^anf-rename-expr())
            new-args = for fold(curr from start, arg from args):
              new-arg  = arg.rename(gensym(arg.id + "."))
              new-body = curr.b^rename-expr(arg, new-arg)
              H.pair(link(new-arg, curr.a), new-body)
            end
            N.a-cases-branch(l2, name, new-args.a, new-args.b)
        end
      end
      new-else     = cases(Option<N.AExpr>) _else:
                       | some(e) =>
                         some(anf-rename-expr(e))
                       | none    =>
                         none
                     end
      N.a-cases(l, type, val, new-branches, new-else)
    | a-lettable(e) =>
      N.a-lettable(e^anf-rename-lettable())
  end
end

fun anf-rename-prog(prog :: N.AProg) -> N.AProg:
  cases(N.AProg) prog:
    | a-program(l, _import, body) =>
      N.a-program(l, _import, anf-rename-expr(body))
  end
end
