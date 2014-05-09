#lang pyret

provide *

data FCmp:
  | False
  | Oeq
  | Ogt
  | Oge
  | Olt
  | Ole
  | One
  | Ord
  | Uno
  | Ueq
  | Ugt
  | Uge
  | Ult
  | Ule
  | Une
  | True
sharing:
  tostring(self) -> String:
    cases(FCmp) self:
      | False =>
        "false"
      | Oeq   =>
        "oeq"
      | Ogt   =>
        "ogt"
      | Oge   =>
        "oge"
      | Olt   =>
        "olt"
      | Ole   =>
        "ole"
      | One   =>
        "one"
      | Ord   =>
        "ord"
      | Uno   =>
        "uno"
      | Ueq   =>
        "ueq"
      | Ugt   =>
        "ugt"
      | Uge   =>
        "uge"
      | Ult   =>
        "ult"
      | Ule   =>
        "ule"
      | Une   =>
        "une"
      | True  =>
        "true"
    end
  end
end
