#lang pyret

provide *

import "ast-h.arr" as AH
import "ast-anf.arr"     as AN
import "ast-lower.arr"   as AL
import "helpers.arr"     as H

fun identity(a): a end

fun h-lettable-to-lower(e :: AH.HLettable, plug :: (AL.Lettable -> AL.Expression)) -> AL.Expression:
  cases(AH.HLettable) e:
    | h-undefined             => plug(AL.l-undefined)
    | h-box(id)               => raise("h-box not handled")
    | h-id(id)                => plug(AL.l-id(id))
    | h-unbox(id)             => raise("h-unbox not handled")
    | h-data(name, closure)   => raise("h-data not handled")
    | h-lam(f, closure)       => raise("h-lam not handled")
    | h-app(f, args)          => plug(AL.l-application(f, args))
    | h-obj(fields)           =>
      new-copy = gensym("table-copy")
      updates  = fields.foldr(fun(field, next):
        cases(AH.HField) field:
          | h-field(field-name, value) => 
            AL.l-seq(AL.l-update(new-copy, field-name, value), next)
        end
      end, plug(AL.l-id(new-copy)))
      AL.l-let(new-copy, AL.l-copy("global.empty-table"), updates)
    | h-update(table, fields) =>
      fields.foldr(fun(field, next):
        cases(AH.HField) field:
          | h-field(field-name, value) => 
            AL.l-seq(AL.l-update(table, field-name, value), next)
        end
      end, plug(AL.l-undefined))
    | h-extend(table, fields) =>
      new-copy = gensym("table-copy")
      updates  = fields.foldr(fun(field, next):
        cases(AH.HField) field:
          | h-field(field-name, value) => 
            AL.l-seq(AL.l-update(new-copy, field-name, value), next)
        end
      end, plug(AL.l-id(new-copy)))
      AL.l-let(new-copy, AL.l-copy(table), updates)
    | h-dot(obj, field)       => plug(AL.l-lookup(obj, field))
    | h-colon(obj, field)     => plug(AL.l-lookup(obj, field))
    | h-get-bang(obj, field)  => plug(AL.l-lookup(obj, field))
  end
end

fun h-expr-to-lower(e :: AH.HExpr, adts :: List<AL.ADT>, plug :: (AL.Expression -> AL.Expression)) -> AL.Expression:
  cases(AH.HExpr) e:
    | h-ret(id)                  =>
    | h-let(bind, val, body)     => h-lettable-to-lower(val, fun(lettable):
        plug(h-expr-to-lower(body, adts, fun(expr):
          AH.l-let(bind, lettable, expr)
        end))
      end)
    | h-assign(bind, val, body)  => h-expr-to-lower(body, fun(expr):
        AL.l-assign(bind, val, expr)
      end)
    | h-try(body, bind, _except) => raise("exception handling not yet implemented")
    | h-if(cond, consq, altern)  =>
  end
end

fun h-proc-to-lower(proc :: AH.NamedFunc, adts :: List<AL.ADT>) -> AL.Procedure:
  cases(AH.NamedFunc) proc:
    | named-func(name, args, body, ret) => 
      l-body = h-expr-to-lower(body, adts, identity)
      AL.l-proc(name, args, ret, l-body)
  end
end

fun h-variant-member-to-lower(member :: AN.AVariantMember) -> AL.VariantMember:
  # TODO: Some of this may need an overhaul
  cases(AN.AVariantMember) member:
    | a-variant-member(l, member-type, bind) =>
      AL.l-variant-member(bind.name)
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
      var count = 1
      AL.l-adt(for map(variant from variants):
        h-variant-to-lower(variant, count)
        count := count + 1
      end)
  end
end

fun h-to-lower(prog) -> AL.Program:

  adts = for map(adt from prog.datas):
    h-adt-to-lower(adt)
  end
  constants = [] # TODO: Constants should be numbers and strings
  procs = for map(func from prog.funcs):
    h-proc-to-lower(func, adts)
  end
  AL.l-prog(constants, procs, adts)
end
