#lang pyret

provide *

# This file and its contents are based on the types found in the OCaml LLVM
# bindings. They have been adapted to Pyret for this compiler.

import "kind.arr" as K
import "icmp.arr" as I
import "fcmp.arr" as F

data Linkage:
  | External
  | Available_externally
  | Link_once
  | Link_once_odr
  | Link_once_odr_auto_hide
  | Weak
  | Weak_odr
  | Appending
  | Internal
  | Private
  | Dllimport
  | Dllexport
  | External_weak
  | Ghost
  | Common
  | Linker_private
  | Linker_private_weak
end

data Visibility:
  | Default
  | Hidden
  | Protected
end


CallConv = {
    c : 0,
    fast : 8,
    cold : 9,
    x86_stdcall : 64,
    x86_fastcall : 65
}

data Attribute:
  | Zext
  | Sext
  | Noreturn
  | Inreg
  | Structret
  | Nounwind
  | Noalias
  | Byval
  | Nest
  | Readnone
  | Readonly
  | Noinline
  | Alwaysinline
  | Optsize
  | Ssp
  | Sspreq
  | Alignment(n :: Number)
  | Nocapture
  | Noredzone
  | Noimplicitfloat
  | Naked
  | Inlinehint
  | Stackalignment(n :: Number)
  | ReturnsTwice
  | UWTable
  | NonLazyBind
end

data Opcode:
  | Invalid # not an instruction
  # Terminator Instructions
  | Ret
  | Br
  | Switch
  | IndirectBr
  | Invoke
  | Invalid2
  | Unreachable
  # Standard Binary Operators
  | Add
  | FAdd
  | Sub
  | FSub
  | Mul
  | FMul
  | UDiv
  | SDiv
  | FDiv
  | URem
  | SRem
  | FRem
  # Logical Operators 
  | Shl
  | LShr
  | AShr
  | And
  | Or
  | Xor
  # Memory Operators
  | Alloca(typ :: K.TypeKind) with: 
    tostring(self): "alloca " + self.typ.tostring() end
  | Load(typ :: K.TypeKind, ptr :: K.ValueKind) with: 
    tostring(self): "load " + typ.tostring() + " " + ptr.tostring() end
  | Store(value-typ :: K.TypeKind, value :: K.ValueKind, ptr-typ :: K.TypeKind, ptr :: K.TypeKind<K.is-Pointer>) with: 
    tostring(self): 
      # TODO figure stuff out first
    end
  | GetElementPtr
  # Cast Operators
  | Trunc
  | ZExt
  | SExt
  | FPToUI
  | FPToSI
  | UIToFP
  | SIToFP
  | FPTrunc
  | FPExt
  | PtrToInt
  | IntToPtr
  | BitCast
  # Other Operators
  | ICmp(cond :: I.Icmp, type :: K.TypeKind, op1 :: K.ValueKind, op2 :: K.ValueKind)
  | FCmp(cond :: F.Fcmp, type :: K.TypeKind, op1 :: K.ValueKind, op2 :: K.ValueKind)
  | PHI
  | Call
  | Select
  | UserOp1
  | UserOp2
  | VAArg
  | ExtractElement
  | InsertElement
  | ShuffleVector
  | ExtractValue
  | InsertValue
  | Fence
  | AtomicCmpXchg
  | AtomicRMW
  | Resume
  | LandingPad
end

data LandingPadClauseTy:
  | Catch
  | Filter
end

data ThreadLocalMode:
  | None
  | GeneralDynamic
  | LocalDynamic
  | InitialExec
  | LocalExec
end
