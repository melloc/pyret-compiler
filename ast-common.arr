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
    when is-t-blank(new-ty):
      raise("Tried to re-type something to t-blank!")
    end
    new-bind = cases (Bind) self:
      | c-bind(id, ty) => c-bind(id, _)
      | c-bind-loc(l, id, ty) => c-bind-loc(l, id, _)
    end
    cases (T.Type) self.ty:
      | t-blank => mk-bind(new-ty)
      | t-any => # TODO retype this too?
      | else => # TODO maybe just return original? 
    end
  end
end

data Field:
  | c-field-name(name :: String)
end

data Global:
  | c-str(name :: Bind, val :: String)
  | c-num(name :: Bind, val :: Number)
end
