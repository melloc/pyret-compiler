#lang pyret

provide *

data AtomicRMWBinOp:
  | Xchg
  | Add
  | Sub
  | And
  | Nand
  | Or
  | Xor
  | Max
  | Min
  | UMax
  | UMin
end

data AtomicOrdering:
  | NotAtomic
  | Unordered
  | Monotonic
  | Acquire
  | Release
  | AcquireRelease
  | SequentiallyConsistent
sharing:
  tostring(self) -> String:
    cases(AtomicOrdering) self:
      | NotAtomic =>
        ""
      | Unordered =>
        "unordered"
      | Monotonic =>
        "monotonic"
      | Acquire =>
        "acquire"
      | Release =>
        "release"
      | AcquireRelease =>
        "acq_rel"
      | SequentiallyConsistent =>
        "seq_cst"
    end
  end
end
