#lang pyret

provide *

import "types.arr" as T
import "ast-common.arr" as AC
import "ast-anf.arr" as AN

# Data definition for H constructs
# Let's have tostring() functions that print out a surface representation.
# We'll need some way of handling depth for that. We'll deal with that later. 

data HLettable:
  | h-undefined
  | h-box(id :: AC.Bind)
  | h-id(id :: AC.Bind)
  | h-unbox(id :: AC.Bind)
  | h-lam(f :: AC.Bind, closure :: AC.Bind)
  | h-app(f :: AC.Bind, args :: List<AC.Bind>)
  | h-obj(fields :: List<HField>)
  | h-update(super :: AC.Bind, fields :: List<HField>)
  | h-extend(super :: AC.Bind, fields :: List<HField>)
  | h-env(field :: AC.Field)
  | h-dot(obj :: AC.Bind, field :: AC.Field)
  | h-colon(obj :: AC.Bind, field :: AC.Field)
  | h-get-bang(obj :: AC.Bind, field :: AC.Field)
sharing:
  rename(self, _from :: AC.Bind, to :: AC.Bind) -> HLettable:
    rename-input = maybe-rename(_, _from, to)
    cases(HLettable) self:
      | h-undefined => self
      | h-box(id) =>
        h-box(rename-input(id))
      | h-id(id) =>
        h-id(rename-input(id))
      | h-unbox(id) =>
        h-unbox(rename-input(id))
      | h-lam(f, closure) =>
        h-lam(rename-input(f), closure)
      | h-app(f, args) =>
        h-app(rename-input(f), args.map(rename-input))
      | h-obj(fields :: List<HField>) =>
        h-obj(maybe-rename-hfields(fields, _from, to))
      | h-update(obj, fields) =>
        new-obj    = maybe-rename(obj, _from, to)
        new-fields = maybe-rename-hfields(obj, _from, to)
        h-update(new-obj, new-fields)
      | h-extend(obj, fields) =>
        new-obj    = maybe-rename(obj, _from, to)
        new-fields = maybe-rename-hfields(obj, _from, to)
        h-extend(new-obj, new-fields)
      | h-env(field) =>
        self
      | h-dot(obj, field) =>
        new-obj = maybe-rename(obj, _from, to)
        h-dot(new-obj, field)
      | h-colon(obj, field) =>
        new-obj = maybe-rename(obj, _from, to)
        h-colon(new-obj, field)
      | h-get-bang(obj, field) =>
        new-obj = maybe-rename(obj, _from, to)
        h-get-bang(new-obj, field)
    end
  end
end

data HCasesBranch:
  | h-cases-branch(name :: String, args :: List<AC.Bind>, body :: HExpr)
end

fun maybe-rename(old :: AC.Bind, _from :: AC.Bind, to :: AC.Bind) -> AC.Bind:
  if old == _from:
    to
  else:
    old
  end
end

fun maybe-rename-hfields(fields :: List<HField>, rename-input :: (AC.Bind, AC.Bind, AC.Bind -> AC.Bind)) -> List<HField>:
  for map(field from fields):
    cases(HField) field:
      | h-field(name, value) =>
        h-field(name, rename-input(value))
    end
  end
end

# We are, for now, just saving the binds as such.
data HExpr:
  | h-ret(id :: AC.Bind)
  | h-let(bind :: AC.Bind, val :: HLettable, body :: HExpr)
  | h-assign(bind :: AC.Bind, val :: AC.Bind, body :: HExpr)
  | h-try(body :: HExpr, bind :: AC.Bind, _except :: HExpr)
  | h-if(c :: AC.Bind, t :: HExpr, e :: HExpr)
  | h-cases(type :: T.Type, val :: AC.Bind, branches :: List<HCasesBranch>, _else :: Option<HExpr>)
sharing:
  rename(self, _from :: AC.Bind, to :: AC.Bind) -> HExpr:
    cases(HExpr) self:
      | h-ret(id) =>
        h-ret(maybe-rename(id, _from, to))
      | h-let(bind, val, body) =>
        new-bind = maybe-rename(bind, _from, to)
        new-val  = val.rename(_from, to)
        new-body = body.rename(_from, to)
        h-let(new-bind, new-val, new-body)
      | h-assign(bind, val, body) =>
        new-bind = maybe-rename(bind, _from, to)
        new-val  = maybe-rename(val, _from, to)
        new-body = body.rename(_from, to)
        h-assign(new-bind, new-val, new-body)
      | h-try(body, bind, _except) =>
        new-body = body.rename(_from, to)
        new-bind = maybe-rename(bind, _from, to)
        new-except = _except.rename(_from, to)
        h-try(new-body, new-bind, new-except)
      | h-if(c, t, e) =>
        new-c = maybe-rename(c, _from, to)
        new-t = t.rename(_from, to)
        new-e = e.rename(_from, to)
        h-if(new-c, new-t, new-e)
      | h-cases(type, val, branches, _else) =>
        new-val = maybe-rename(val, _from, to)
        new-branches = for map(branch from branches): branch.rename(_from, to) end
        new-else = cases(Option<HExpr>) _else:
          | none => none
          | some(e) => e.rename(_from, to)
        end
        h-cases(type, new-val, new-branches, new-else)
    end
  end
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
  h-field(name :: AC.Field, value :: AC.Bind)
end
# The value of a field can be a string, since we lift the lookup out 
# beforehand. If that lookup is in fact just an id, we will substitute
# it back in before generating code. 


# Internal data type for representing data expressions
data NamedData:
  | named-data(name :: String, 
               variants :: List<HVariant>, 
               shared :: List<HField>,
               closure :: Set<String>)
end

data NamedFunc:
  | named-func(name :: String, 
               args :: List<AC.Bind>, 
               body :: HExpr,
               ret :: T.Type,
               is-closure :: Boolean)
end
