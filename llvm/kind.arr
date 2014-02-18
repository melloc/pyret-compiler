#lang pyret

provide *

import "llvm.arr" as LLVM

data TypeKind:
  | Void
  | Half
  | Float
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128
  | Label
  | Integer
  | FunctionType
  | Struct
  | Arr
  | Pointer
  | Vector
  | Metadata
  | X86_mmx
end

data ValueKind:
  | NullValue
  | Argument
  | BasicBlock
  | InlineAsm
  | MDNode
  | MDString
  | BlockAddress
  | ConstantAggregateZero
  | ConstantArray
  | ConstantDataArray
  | ConstantDataVector
  | ConstantExpr
  | ConstantFP
  | ConstantInt
  | ConstantPointerNull
  | ConstantStruct
  | ConstantVector
  | FunctionValue
  | GlobalAlias
  | GlobalVariable
  | UndefValue
  | Instruction(op :: LLVM.Opcode)
end
