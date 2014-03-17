#lang pyret

provide *

# This file and its contents are based on the types found in the OCaml LLVM
# bindings. They have been adapted to Pyret for this compiler.

import "kind.arr" as K
import "icmp.arr" as I
import "fcmp.arr" as F

data ProcedureBlock:
  | Procedure(type :: K.TypeKind<K.is-FunctionType>, instructions :: List<Opcode>)
end

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

data VLPair:
  # TODO what type should the label be?
  | ValueLabelPair(value :: K.ValueType, label :: String) with:
    tostring(self):
      "[ " + self.value.tostring() + ", " + self.label + " ]"
    end
end

data Instruction:
  | Assign(name :: String, op :: OpCode)
  | NoAssign(op :: OpCode)
  | Label(name :: String)
end

data SwitchBranch:
  | switch-branch(intty :: K.TypeKind, value, label :: String)
end

data FunctionAttribute:
  | NoReturn
  | NoUnwind
  | ReadOnly
  | ReadNone
end

data OpCode:
  | Invalid # not an instruction
  # Terminator Instructions
  | Ret(typ :: K.TypeKind, value :: Option<K.ValueKind>) with:
    tostring(self):
      "ret " + self.typ.tostring()
        + cases (Option<K.ValueKind>) self.value:
            | none => ""
            | some(val) => " " + val.tostring()
          end
    end
  | BrConditional(cond-id :: String, consq-label :: String, altern-label :: String)
  | BrUnconditional(dest-label :: String)
  | Switch(intty :: K.TypeKind, value, default :: String, branches :: List<SwitchBranch>)
  | IndirectBr
  | Invoke
  | Invalid2
  | Unreachable
  # Standard Binary Operators
  | Add(nuw :: Bool,
        nsw :: Bool,
        typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind) with:
    tostring(self):
      "add " + if self.nuw: "nuw " else: "" end
        + if self.nsw: "nsw " else: "" end
        + self.typ.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | FAdd
  | Sub(nuw :: Bool,
        nsw :: Bool,
        typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind) with:
    tostring(self):
      "sub " + if self.nuw: "nuw " else: "" end
        + if self.nsw: "nsw " else: "" end
        + self.typ.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | FSub
  | Mul(nuw :: Bool,
        nsw :: Bool,
        typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind) with:
    tostring(self):
      "mul " + if self.nuw: "nuw " else: "" end
        + if self.nsw: "nsw " else: "" end
        + self.typ.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | FMul
  | UDiv(exact :: Bool,
         typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind) with:
    tostring(self):
      "udiv " + if self.exact: "exact " else: "" end
        + self.type.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | SDiv(exact :: Bool,
         typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind) with:
    tostring(self):
      "sdiv " + if self.exact: "exact " else: "" end
        + self.type.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | FDiv
  | URem(typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind) with:
    tostring(self):
      "urem " + self.type.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | SRem(typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind) with:
    tostring(self):
      "srem " + self.type.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | FRem
  # Logical Operators
  | Shl(nuw :: Bool,
        nsw :: Bool,
        typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind) with:
    tostring(self):
      "shl " + if self.nuw: "nuw " else: "" end
        + if self.nsw: "nsw " else: "" end
        + self.typ.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | LShr(exact :: Bool,
         typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind) with:
    tostring(self):
      "lshr " + if self.exact: "exact " else: "" end
        + self.type.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | AShr(exact :: Bool,
         typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind) with:
    tostring(self):
      "ashr " + if self.exact: "exact " else: "" end
        + self.type.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | And(typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind) with:
    tostring(self):
      "and " + self.type.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | Or(typ :: K.TypeKind,
       op1 :: K.ValueKind,
       op2 :: K.ValueKind) with:
    tostring(self):
      "or " + self.type.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | Xor(typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind) with:
    tostring(self):
      "xor " + self.type.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  # Memory Operators
  | Alloca(typ :: K.TypeKind) with:
    tostring(self): "alloca " + self.typ.tostring() end
  | Load(typ :: K.TypeKind, ptr :: K.ValueKind) with:
    tostring(self): "load " + self.typ.tostring() + " " + self.ptr.tostring() end
  | Store(volatiile :: Bool,  # TODO let's make atomic stores a different type
          value-typ :: K.TypeKind,
          value :: K.ValueKind,
          ptr-typ :: K.TypeKind,
          ptr :: K.ValueKind,
          alignment :: Option<Number>,
          nontemporal :: Option<K.ValueKind>) with:
    tostring(self):
      "store " + if self.volatile: "volatile " else: "" end
        + self.value-typ.tostring() + " " + self.value.tostring() + ", "
        + self.ptr-typ.tostring() + " " + self.ptr.tostring()
        + cases (Option<Number>) self.alignment:
            | none => ""
            | some(val) => ", align " + val.tostring()
          end
        + cases (Option<K.ValueKind>) self.nontemporal:
            | none => ""
            | some(val) => " !nontemporal !" + val.tostring()
          end
    end
  | StoreAtomic(volatiile :: Bool,  # TODO let's make atomic stores a different type
                value-typ :: K.TypeKind,
                value :: K.ValueKind,
                ptr-typ :: K.TypeKind,
                ptr :: K.ValueKind,
                singlethread :: Bool)
                #ordering ::
                #alignment ::
                # TODO TODO TODO
  | GetElementPtr(inbound :: Bool, pty :: K.TypeKind, val, access :: List<Pair<K.TypeKind, Number>>)
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
  | ICmp(cond :: I.Icmp,
         typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind) with:
    tostring(self):
      "icmp " + self.cond.tostring() + " " + self.typ.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | FCmp(cond :: F.Fcmp,
         typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind) with:
    tostring(self):
      "fcmp " + self.cond.tostring() + " " + self.typ.tostring() + " "
        + self.op1.tostring() + ", " + self.op2.tostring()
    end
  | PHI(typ :: K.TypeKind,
        pairs :: List<Pair<K.TypeKind,String>>) with:
    tostring(self):
      "phi " + self.typ.tostring() + " " + self.pairs.join-str(", ")
    end
  | Call(tail :: Bool,
         cconv  :: String,
         #attrs :: List<Attribute>, # TODO not something else?
         retty  :: K.TypeKind,
         func   :: String,
         args   :: List<ArgPair>,
         fattrs :: List<FunctionAttribute>) with:
    tostring(self):
      "call " + if self.tail: "tail " else: "" end
        + self.cconv + " " # TODO change this
        + self.ret.tostring() + " " + self.func.tostring() + "("
        + self.args.join-str(", ") + ")"
    end
  | Select(selty :: K.TypeKind,
           cond :: K.ValueKind,
           typ1 :: K.TypeKind,
           val1 :: K.ValueKind,
           typ2 :: K.TypeKind,
           val2 :: K.ValueKind) with:
    tostring(self):
      selty-str = self.selty.tostring() # TODO TODO TODO
      "select " + selty-str + " " + self.cond.tostring() + ", "
        + self.typ1.tostring() + " " + self.val1.tostring() + ", "
        + self.typ2.tostring() + " " + self.val2.tostring()
    end
  | UserOp1
  | UserOp2
  | VAArg
  | ExtractElement
  | InsertElement
  | ShuffleVector
  | ExtractValue(agtyp :: K.TypeKind, val :: K.ValueKind, idxs :: List) # TODO
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
