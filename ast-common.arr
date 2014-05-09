#lang pyret

provide *

import ast as A
import "types.arr" as T

Loc = error.Location

data Bind:
  | c-bind(id :: String, ty :: T.Type)
  | c-bind-loc(l :: Loc, id :: String, ty :: T.Type)
sharing:
  rename(self, new-id :: String) -> Bind:
    cases(Bind) self:
      | c-bind(id, ty) => c-bind(new-id, ty)
      | c-bind-loc(l, id, ty) => c-bind-loc(l, new-id, ty)
    end
  end,
  retype-if-blank(self, new-ty :: T.Type) -> Bind:
    when T.is-t-blank(new-ty):
      raise("Tried to re-type something to t-blank!")
    end
    mk-bind = cases (Bind) self:
      | c-bind(id, ty) => c-bind(id, _)
      | c-bind-loc(l, id, ty) => c-bind-loc(l, id, _)
    end
    cases (T.Type) self.ty:
      | t-blank => mk-bind(new-ty)
      | t-any => mk-bind(new-ty)
      | else => self 
    end
  end,
  _equals(self :: Bind, other :: Bind) -> Boolean:
    self.id == other.id
  end,
  _lessthan(self :: Bind, other :: Bind) -> Boolean:
    self.id < other.id
  end
end

data Field:
  | c-field-name(name :: String)
end

data Global:
  | c-str(name :: Bind, val :: String)
  | c-num(name :: Bind, val :: Number)
sharing:
  _lessthan(self :: Global, other :: Global) -> Boolean:
    self.name < other.name
  end
end
