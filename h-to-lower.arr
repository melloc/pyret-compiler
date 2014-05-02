#lang pyret

provide *

import ast               as A
import "types.arr"       as T
import "ast-common.arr"  as AC
import "ast-h.arr"       as AH
import "ast-anf.arr"     as AN
import "ast-lower.arr"   as AL
import "helpers.arr"     as H

fun identity(a): a end

fun h-lettable-to-lower(e :: AH.HLettable, plug :: (AL.Lettable -> AL.Expression)) -> AL.Expression:
  cases(AH.HLettable) e:
    | h-undefined             => plug(AL.l-undefined)
    | h-id(id)                => raise("There should be no h-id's left when converting to the lower AST!")
    | h-box(id)               => plug(AL.l-box(id))
    | h-unbox(id)             => plug(AL.l-unbox(id))
    | h-lam(f, env)           => plug(AL.l-val(AL.l-closure(f, env)))
    | h-app(f, args)          => plug(AL.l-application(f, args))
    | h-obj(fields)           =>
      empty-table = AL.l-copy(AC.c-bind("global.empty-table", T.t-record([])))
      cases(List<AH.HField>) fields:
        | empty      => 
          plug(empty-table)
        | link(f, r) =>
          new-copy = AC.c-bind(gensym("table-copy"), T.t-record([]))
          first-update = cases(AH.HField) f:
            | h-field(field-name, value) => 
              AL.l-update(new-copy, field-name, value)
          end
          updates  = r.foldr(fun(field, next):
            cases(AH.HField) field:
              | h-field(field-name, value) =>
                AL.l-seq(AL.l-update(new-copy, field-name, value), next)
            end
          end, plug(first-update))
          AL.l-let(new-copy, empty-table, updates)
      end
    | h-update(table, fields) =>
      fields.foldr(fun(field, next):
        cases(AH.HField) field:
          | h-field(field-name, value) =>
            AL.l-seq(AL.l-update(table, field-name, value), next)
        end
      end, plug(AL.l-undefined))
    | h-extend(table, fields) =>
      new-copy = gensym("table-copy")
      table-copy = AL.l-copy(table)
      cases(List<HField>) fields:
        | empty      => plug(table-copy)
        | link(f, r) =>
          first-update = cases(AH.HField) f:
            | h-field(field-name, value) => 
              AL.l-update(new-copy, field-name, value)
          end
          updates = r.foldr(fun(field, next):
            cases(AH.HField) field:
              | h-field(field-name, value) =>
                AL.l-seq(AL.l-update(new-copy, field-name, value), next)
            end
          end, plug(first-update))
          AL.l-let(new-copy, table-copy, updates)
      end
    | h-dot(obj, field)       => plug(AL.l-lookup(obj, field))
    | h-colon(obj, field)     => plug(AL.l-lookup(obj, field))
    | h-get-bang(obj, field)  => plug(AL.l-lookup(obj, field))
    | h-env(field)            => plug(AL.l-env(field))
  end
end

fun h-expr-to-lower(e :: AH.HExpr, adts :: List<AL.ADT>, plug :: (AL.Expression -> AL.Expression)) -> AL.Expression:
  cases(AH.HExpr) e:
    | h-ret(id)                  => plug(AL.l-ret(id))
    | h-let(bind, val, body)     =>
      h-lettable-to-lower(val, fun(lettable :: AL.Lettable):
        plug(h-expr-to-lower(body, adts, fun(expr :: AL.Expression):
          AL.l-let(bind, lettable, expr)
        end))
      end)
    | h-assign(bind, val, body)  => h-expr-to-lower(body, adts, fun(expr :: AL.Expression):
        AL.l-assign(bind, val, expr)
      end)
    | h-try(body, bind, _except) => raise("exception handling not yet implemented")
    | h-if(cond, consq, altern)  =>
      h-expr-to-lower(consq, adts, fun(lower-consq :: AL.Expression):
        h-expr-to-lower(altern, adts, fun(lower-altern :: AL.Expression):
          plug(AL.l-if(cond, lower-consq, lower-altern))
        end)
      end)
    | h-cases(type, val, branches, _else) =>
      adt = AL.find-adt(type, adts)
      lower-branches = for map(branch from branches):
        cases(AH.HCasesBranch) branch:
          | h-cases-branch(name, args, body) =>
            variant = adt.lookup-variant(name)
            lower-body = h-expr-to-lower(body, adts, identity)
            joined = H.zip(args, variant.fields)
            new-body = for fold(current from lower-body, pair from joined):
              cases(H.Pair) pair:
                | pair(binding, field) =>
                  AL.l-let(binding, AL.l-lookup(val, field), current)
              end
            end
            AL.l-branch(variant.tag, new-body)
        end
      end
      lower-else = cases(Option<AH.HExpr>) _else:
        | some(else-hexpr) => 
          h-expr-to-lower(else-hexpr, adts, identity)
        | none    =>
          AL.l-exit("No usable case available: " + type.tostring())
      end
      plug(AL.l-switch(val, lower-branches, lower-else))
  end
end

fun h-proc-to-lower(proc :: AH.NamedFunc, adts :: List<AL.ADT>) -> AL.Procedure:
  cases(AH.NamedFunc) proc:
    | named-func(name, args, body, ret, is-closure) =>
      l-body = h-expr-to-lower(body, adts, identity)
      AL.l-proc(name, args, ret, l-body, is-closure)
  end
end

fun h-variant-member-to-lower(member :: AN.AVariantMember) -> AC.Field:
  # TODO: Some of this may need an overhaul
  cases(AN.AVariantMember) member:
    | a-variant-member(l, member-type, bind) =>
      AC.c-field-name(bind.id)
  end
end

fun h-variant-to-lower(variant :: AH.HVariant, count :: Number) -> AL.Variant:
  cases(AH.HVariant) variant:
    | h-variant(name, members, with-members)  =>
      AL.l-variant(name, AL.Tagged(count), for map(member from members):
        h-variant-member-to-lower(member)
      end)
    | h-singleton-variant(name, with-members) =>
      AL.l-variant(name, AL.Constant(count), empty)
  end
end

fun h-adt-to-lower(adt :: AH.NamedData) -> AL.ADT:
  cases(AH.NamedData) adt:
    | named-data(name, variants, shared, closure) =>
      var count = 0
      AL.l-adt(name, for map(variant from variants):
        count := count + 1
        h-variant-to-lower(variant, count)
      end)
  end
end

fun h-to-lower(prog) -> AL.Program:

  adts = for map(adt from prog.datas):
    h-adt-to-lower(adt)
  end
  procs = for map(func from prog.funcs):
    h-proc-to-lower(func, adts)
  end
  
  AL.l-prog(prog.globals, procs, adts, h-expr-to-lower(prog.expr, adts, identity))
end
