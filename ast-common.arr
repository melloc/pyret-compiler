#lang pyret

provide *

import ast as A

Loc = error.Location

data Bind:
  | c-bind(id :: String, ann :: A.Ann)
  | c-bind-loc(l :: Loc, id :: String, ann :: A.Ann)
sharing:
  rename(self, new-id :: String) -> Bind:
    cases(Bind) self:
      | c-bind(id, ann) => c-bind(new-id, ann)
      | c-bind-loc(l, id, ann) => c-bind-loc(l, new-id, ann)
    end
  end
end
