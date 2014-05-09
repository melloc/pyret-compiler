#lang pyret

provide *

data ICmp:
  | Eq
  | Ne
  | Ugt
  | Uge
  | Ult
  | Ule
  | Sgt
  | Sge
  | Slt
  | Sle
sharing:
  tostring(self) -> String:
    cases(ICmp) self:
      | Eq  =>
        "eq"
      | Ne  =>
        "ne"
      | Ugt =>
        "ugt"
      | Uge =>
        "uge"
      | Ult =>
        "ult"
      | Ule =>
        "ule"
      | Sgt =>
        "sgt"
      | Sge =>
        "sge"
      | Slt =>
        "slt"
      | Sle =>
        "sle"
    end
  end
end
