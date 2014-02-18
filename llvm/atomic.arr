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
  | Invalid
  | Acquire
  | Release
  | AcqiureRelease
  | SequentiallyConsistent
end
