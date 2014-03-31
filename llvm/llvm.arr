#lang pyret

provide *

# This file and its contents are based on the types found in the OCaml LLVM
# bindings. They have been adapted to Pyret for this compiler.

import "kind.arr" as K
import "icmp.arr" as I
import "fcmp.arr" as F
import "../helpers.arr" as H

data ModuleBlock:
  | Module(constants :: List<Constant>, procedures :: List<ProcedureBlock>)
sharing:
  tostring(self) -> String:
    cases(ModuleBlock) self:
      | Module(constants, procedures) =>
        constant-string = for map(constant from constants):
          constant.tostring()
        end.join-str("\n")
        procedure-string = for map(procedure from procedures):
          procedure.tostring()
        end.join-str("\n")
        constant-string + "\n" + procedure-string
    end
  end
end


data ProcedureBlock:
  | Procedure(name         :: String,
              ret-type     :: K.TypeKind,
              params       :: List<K.TypeKindField>,
              instructions :: List<Instruction>)
sharing:
  tostring(self) -> String:
    cases(ProcedureBlock) self:
      | Procedure(name, type, params, instructions) =>
        "define " + type.tostring() + " @" + name + "("
          + (for map(param from params):
              cases(K.TypeKindField) param:
                | TypeField(param-name, kind) => kind.tostring() + " %" + param-name
              end
            end).join(", ")
          + ") {\n"
          + for fold(base from "", instruction from instructions):
              base + instruction.tostring() + "\n"
            end
          + "}\n"
    end
  end
end


data CallingConvention:
  | CCC
  | FastCC
  | ColdCC
  | CC(n :: Number)
  | WebKitJSCC
  | AnyRegCC
  | PreserveMostCC
  | PreserveAllCC
sharing:
  tostring(self):
    cases(CallingConvention) self:
      | CCC            => "ccc"
      | FastCC         => "fastcc"
      | ColdCC         => "coldcc"
      | CC(n)          => "cc " + n.tostring()
      | WebKitJSCC     => "webkit_jscc"
      | AnyRegCC       => "anyregcc"
      | PreserveMostCC => "preserve_mostcc"
      | PreserveAllCC  => "preserve_allcc"
    end
  end
end

data Linkage:
  | External
  | AvailableExternally
  | LinkOnce
  | LinkOnceODR
  | LinkOnceODRAutoHide
  | Weak
  | WeakODR
  | Appending
  | Internal
  | Private
  | DLLImport
  | DLLExport
  | ExternalWeak
  | Common
  | LinkerPrivate
  | LinkerPrivateWeak
sharing:
  tostring(self) -> String:
    cases(Linkage) self:
      | External            => "external"
      | AvailableExternally => "available_externally"
      | LinkOnce            => "linkonce"
      | LinkOnceODR         => "linkonce_odr"
      | LinkOnceODRAutoHide => "linkonce_odr_auto_hide"
      | Weak                => "weak"
      | WeakODR             => "weak_odr"
      | Appending           => "appending"
      | Internal            => "internal"
      | Private             => "private"
      | DLLImport           => "dllimport"
      | DLLExport           => "dllexport"
      | ExternalWeak        => "extern_weak"
      | Common              => "common"
      | LinkerPrivate       => "linker_private"
      | LinkerPrivateWeak   => "linker_private_weak"
    end
  end
end

data Visibility:
  | Default
  | Hidden
  | Protected
sharing:
  tostring(self) -> String:
    cases(Visibility) self:
      | Default   => "default"
      | Hidden    => "hidden"
      | Protected => "protected"
    end
  end
end

data FunctionAttribute:
  | NoInline
  | NoReturn
  | NoUnwind
  | AlwaysInline
  | OptSize
  | SanitizeAddress
  | SanitizeMemory
  | SanitizeThread
  | SSP
  | SSPReq
  | SSPStrong
  | Alignment(n :: Number)
  | NoRedzone
  | NoImplicitFloat
  | Naked
  | InlineHint
  | StackAlignment(n :: Number)
  | ReadOnly
  | ReadNone
  | ReturnsTwice
  | UWTable
  | NonLazyBind
sharing:
  tostring(self):
    cases(FunctionAttribute) self:
      | Alignment(n)      => "align(" + n.tostring() + ")"
      | AlwaysInline      => "alwaysinline"
      | InlineHint        => "inlinehint"
      | NoInline          => "noinline"
      | Naked             => "naked"
      | NoImplicitFloat   => "noimplicitfloat"
      | NonLazyBind       => "nonlazybind"
      | NoRedzone         => "noredzone"
      | NoReturn          => "noreturn"
      | NoUnwind          => "nounwind"
      | OptSize           => "optsize"
      | ReadOnly          => "readonly"
      | ReadNone          => "readnone"
      | SanitizeAddress   => "sanitize_address"
      | SanitizeMemory    => "sanitize_memory"
      | SanitizeThread    => "sanitize_thread"
      | SSP               => "ssp"
      | SSPReq            => "sspreq"
      | SSPStrong         => "sspstrong"
      | StackAlignment(n) => "alignstack(" + n.tostring() + ")"
      | ReturnsTwice      => "returns_twice"
      | UWTable           => "uwtable"
    end
  end
end


data ParameterAttribute:
  | ByVal
  | InAlloca
  | InReg
  | Nest
  | NoAlias
  | NoCapture
  | Returned
  | SignExt
  | StructRet
  | ZeroExt
sharing:
  tostring(self):
    cases(ParameterAttribute) self:
      | ByVal             => "byval"
      | InAlloca          => "inalloca"
      | InReg             => "inreg"
      | Nest              => "nest"
      | NoAlias           => "noalias"
      | NoCapture         => "nocapture"
      | Returned          => "returned"
      | SignExt           => "signext"
      | StructRet         => "sret"
      | ZeroExt           => "zeroext"
    end
  end
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
sharing:
  tostring(self) -> String:
    cases(Instruction) self:
      | Assign(name, op) => name + " := " + op.tostring()
      | NoAssign(op)     => op.tostring()
      | Label(name)      => name + ":"
    end
  end
end

data SwitchBranch:
  | switch-branch(intty :: K.TypeKind, value, label :: String)
end

data OpCode:
  | Invalid # not an instruction
  # Terminator Instructions
  | Ret(typ :: K.TypeKind, value :: Option<K.ValueKind>)
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
        op2 :: K.ValueKind)
  | FAdd
  | Sub(nuw :: Bool,
        nsw :: Bool,
        typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind)
  | FSub
  | Mul(nuw :: Bool,
        nsw :: Bool,
        typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind)
  | FMul
  | UDiv(exact :: Bool,
         typ   :: K.TypeKind,
         op1   :: K.ValueKind,
         op2   :: K.ValueKind)
  | SDiv(exact :: Bool,
         typ   :: K.TypeKind,
         op1   :: K.ValueKind,
         op2   :: K.ValueKind)
  | FDiv
  | URem(typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind)
  | SRem(typ :: K.TypeKind,
         op1 :: K.ValueKind,
         op2 :: K.ValueKind)
  | FRem
  # Logical Operators
  | Shl(nuw :: Bool,
        nsw :: Bool,
        typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind)
  | LShr(exact :: Bool,
         typ   :: K.TypeKind,
         op1   :: K.ValueKind,
         op2   :: K.ValueKind)
  | AShr(exact :: Bool,
         typ   :: K.TypeKind,
         op1   :: K.ValueKind,
         op2   :: K.ValueKind)
  | And(typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind)
  | Or(typ :: K.TypeKind,
       op1 :: K.ValueKind,
       op2 :: K.ValueKind)
  | Xor(typ :: K.TypeKind,
        op1 :: K.ValueKind,
        op2 :: K.ValueKind)
  # Memory Operators
  | Alloca(typ :: K.TypeKind)
  | Load(typ :: K.TypeKind, ptr)
  | Store(volatile    :: Bool,  # TODO let's make atomic stores a different type
          value-typ   :: K.TypeKind,
          value       :: K.ValueKind,
          ptr-typ     :: K.TypeKind,
          ptr         :: K.ValueKind,
          alignment   :: Option<Number>,
          nontemporal :: Option<K.ValueKind>)
  | StoreAtomic(volatile :: Bool,  # TODO let's make atomic stores a different type
                value-typ :: K.TypeKind,
                value :: K.ValueKind,
                ptr-typ :: K.TypeKind,
                ptr :: K.ValueKind,
                singlethread :: Bool)
                #ordering ::
                #alignment ::
                # TODO TODO TODO
  | GetElementPtr(inbound :: Bool, pty :: K.TypeKind, val, access :: List<H.Pair<K.TypeKind, Number>>)
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
  | BitCast(from-ty :: K.TypeKind, value, to-ty :: K.TypeKind)
  # Other Operators
  | ICmp(cond :: I.Icmp,
         typ  :: K.TypeKind,
         op1  :: K.ValueKind,
         op2  :: K.ValueKind)
  | FCmp(cond :: F.Fcmp,
         typ  :: K.TypeKind,
         op1  :: K.ValueKind,
         op2  :: K.ValueKind)
  | PHI(typ   :: K.TypeKind, pairs :: List<Pair<K.TypeKind,String>>)
  | Call(tail   :: Bool,
         cconv  :: CallingConvention,
         retty  :: K.TypeKind,
         func   :: String,
         args   :: List<ArgPair>,
         fattrs :: List<FunctionAttribute>)
  | Select(selty :: K.TypeKind,
           cond :: K.ValueKind,
           typ1 :: K.TypeKind,
           val1 :: K.ValueKind,
           typ2 :: K.TypeKind,
           val2 :: K.ValueKind)
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
sharing:
  tostring(self) -> String:
    cases(OpCode) self:
      | Invalid =>
      | Ret(typ, value) =>
        "ret " + typ.tostring()
          + cases(Option<K.ValueKind>) value:
              | none => ""
              | some(val) => " " + val.tostring()
            end
      | BrConditional(cond-id, consq-label, altern-label) =>
        "br i1 " + cond-id + ", label " + consq-label + ", label " + altern-label
      | BrUnconditional(dest-label :: String) =>
        "br " + dest-label
      | Switch(intty, value, default, branches) =>
        "switch " + intty.tostring() + " " + value.tostring() + ", label " + default
          + cases(List) branches:
              | empty => ""
              | link(_, _) =>
                for fold(base from " [ ", current from branches):
                  base + current.tostring()
                end + " ] "
            end
      | IndirectBr =>
      | Invoke =>
      | Invalid2 =>
      | Unreachable =>
        "unreachable"
      | Add(nuw, nsw, typ, op1, op2) =>
        "add "
          + if nuw: "nuw " else: "" end
          + if nsw: "nsw " else: "" end
          + typ.tostring() + " "
          + op1.tostring() + ", " + op2.tostring()
      | FAdd =>
      | Sub(nuw, nsw, typ, op1, op2) =>
        "sub "
          + if nuw: "nuw " else: "" end
          + if nsw: "nsw " else: "" end
          + typ.tostring() + " "
          + op1.tostring() + ", " + op2.tostring()
      | FSub =>
      | Mul(nuw, nsw, typ, op1, op2) =>
        "mul "
          + if nuw: "nuw " else: "" end
          + if nsw: "nsw " else: "" end
          + typ.tostring() + " "
          + op1.tostring() + ", " + op2.tostring()
      | UDiv(exact, typ, op1, op2) =>
        "udiv " + if exact: "exact " else: "" end
          + typ.tostring() + " " + op1.tostring() + ", " + op2.tostring()
      | SDiv(exact, typ, op1, op2) =>
        "sdiv " + if exact: "exact " else: "" end
          + typ.tostring() + " "
          + op1.tostring() + ", " + op2.tostring()
      | FDiv =>
      | URem(typ, op1, op2) =>
        "urem " + typ.tostring() + " " + op1.tostring() + ", " + op2.tostring()
      | SRem(typ, op1, op2) =>
        "srem " + typ.tostring() + " " + op1.tostring() + ", " + op2.tostring()
      | FRem =>
      | Shl(nuw, nsw, typ, op1, op2) =>
        "shl "
          + if nuw: "nuw " else: "" end
          + if nsw: "nsw " else: "" end
          + typ.tostring() + " "
          + op1.tostring() + ", " + op2.tostring()
      | LShr(exact, typ, op1, op2) =>
        "lshr " + if exact: "exact " else: "" end
          + typ.tostring() + " "
          + op1.tostring() + ", " + op2.tostring()
      | AShr(exact, typ, op1, op2) =>
        "ashr " + if exact: "exact " else: "" end
          + typ.tostring() + " "
          + op1.tostring() + ", " + op2.tostring()
      | And(typ, op1, op2) =>
        "and " + typ.tostring() + " " + op1.tostring() + ", " + op2.tostring()
      | Or(typ, op1, op2) =>
        "or " + typ.tostring() + " " + op1.tostring() + ", " + op2.tostring()
      | Xor(typ, op1, op2) =>
        "xor " + typ.tostring() + " " + op1.tostring() + ", " + op2.tostring()
      | Alloca(typ) =>
        "alloca " + self.typ.tostring()
      | Load(typ, ptr) =>
        "load " + typ.tostring() + " " + ptr.tostring()
      | Store(volatile, value-typ, value, ptr-typ, ptr, alignment, nontemporal) =>
        "store " + if volatile: "volatile " else: "" end
          + value-typ.tostring() + " " + value.tostring() + ", "
          + ptr-typ.tostring() + " " + ptr.tostring()
          + cases(Option<Number>) alignment:
              | none => ""
              | some(val) => ", align " + val.tostring()
            end
          + cases(Option<K.ValueKind>) nontemporal:
              | none => ""
              | some(val) => " !nontemporal !" + val.tostring()
            end
      | StoreAtomic(volatile, value-typ, value, ptr-typ, ptr, singlethread) =>
      | GetElementPtr(inbound, pty, val, accesses) =>
        "getelementptr " + if inbound: "inbound " else: "" end + pty.tostring() + "*"
          + for fold(base from "", access from accesses):
              cases(H.Pair) access:
                | pair(ty, idx) => base + ", " + ty.tostring() + " " + idx.tostring()
              end
            end
      | Trunc    =>
      | ZExt     =>
      | SExt     =>
      | FPToUI   =>
      | FPToSI   =>
      | UIToFP   =>
      | SIToFP   =>
      | FPTrunc  =>
      | FPExt    =>
      | PtrToInt =>
      | IntToPtr =>
      | BitCast(from-ty, value, to-ty) =>
        "bitcast " + from-ty.tostring() + " " + value.tostring() + " to " + to-ty.tostring()
      | ICmp(cond, typ, op1, op2) =>
        "icmp " + cond.tostring() + " " + typ.tostring() + " "
          + op1.tostring() + ", " + op2.tostring()
      | FCmp(cond, typ, op1, op2) =>
        "fcmp " + cond.tostring() + " " + typ.tostring() + " "
          + op1.tostring() + ", " + op2.tostring()
      | PHI(typ, pairs) =>
        "phi " + typ.tostring() + " " + pairs.join-str(", ")
      | Call(tail, cconv, retty, func, args, fattrs) =>
        "call " + if tail: "tail " else: "" end
          + cconv.tostring() + " " # TODO change this
          + retty.tostring() + " " + func.tostring() + "("
          + args.join-str(", ") + ")"
      | Select(selty, cond, typ1, val1, typ2, val2) =>
        selty-str = selty.tostring()
        "select " + selty-str + " " + cond.tostring() + ", "
          + typ1.tostring() + " " + val1.tostring() + ", "
          + typ2.tostring() + " " + val2.tostring()
      | UserOp1 =>
      | UserOp2 =>
      | VAArg   =>
      | ExtractElement =>
      | InsertElement  =>
      | ShuffleVector  =>
      | ExtractValue(agtyp, val, idxs)  =>
      | InsertValue    =>
      | Fence          =>
      | AtomicCmpXchg  =>
      | AtomicRMW      =>
      | Resume         =>
      | LandingPad     =>
    end
  end
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
sharing:
  tostring(self) -> String:
    cases(ThreadLocalMode) self:
      | None           => ""
      | GeneralDynamic => "generaldynamic"
      | LocalDynamic   => "localdynamic"
      | InitialExec    => "initialexec"
      | LocalExec      => "localexec"
    end
  end
end

data GlobalMode:
  | GlobalConstant(value :: K.ValueKind)
  | GlobalVariable
end

data Global:
  | GlobalDecl(var-name     :: String,
               linkage      :: Linkage,
               visibility   :: Visibility,
               storage-class,
               thread-local :: ThreadLocalMode,
               mode         :: GlobalMode,
               ty           :: K.TypeKind,
               section      :: Option<String>,
               align        :: Option<Number>)
sharing:
  tostring(self) -> String:
    cases(Global) self:
      | GlobalDecl(var-name, linkage, visibility, storage-class, thread-local,
                   mode, ty, section, align) =>
        "@" + var-name + " = " + linkage.tostring() + " " + storage-class.tostring()
          + thread-local.tostring()
          + cases(GlobalMode) mode:
              | GlobalConstant(value) => "constant " + ty.tostring() + " " + value.tostring()
              | GlobalVariable        => "global " + ty.tostring()
            end
          + cases(Option<String>) section:
              | some(s) => ", section \"" + s + "\""
              | none    => ""
            end
          + cases(Option<Number>) align:
              | some(n) => ", align " + n.tostring()
              | none    => ""
            end
    end
  end
end
