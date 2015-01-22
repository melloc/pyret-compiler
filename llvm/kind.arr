#lang pyret

provide *

# import "llvm.arr" as LLVM
import "icmp.arr" as I
import "fcmp.arr" as F

data TypeKindField:
  | TypeField(name :: String, kind :: TypeKind)
end

data TypeKind:
  | Void
  | Half
  | Float
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128
  | Label
  | Integer(width :: Number)
  | FunctionType(ret :: TypeKind, params :: List<TypeKind>, is-vararg :: Boolean)
  | Struct(fields :: List<TypeKindField>, packed :: Boolean)
  | Arr(len :: Number, typ :: TypeKind)
  | Pointer(typ :: TypeKind, addrspace :: Option<Number>)
  | Vector(len :: Number, typ :: TypeKind)
  | TypeIdentifier(name :: String)
  | Metadata
  | X86_mmx
sharing:
  tostring(self):
    cases(TypeKind) self:
      | Void      => "void"
      | Half      => "half"
      | Float     => "float"
      | Double    => "double"
      | X86fp80   => "x86_fp80"
      | Fp128     => "fp128"
      | Ppc_fp128 => "ppc_fp128"
      | Label     => "label"
      | Integer(width) =>
        "i" + width.tostring()
      | FunctionType(ret, params, is-vararg) =>
        ret.tostring() 
          + " (" 
          + params.join-str(", ") 
          + if is-vararg: ", ..." else: "" end
          + ")*"
      | Struct(fields, packed) =>
        inside = "{ " + fields.join-str(", ") + " }"
        if packed: "<" + inside + ">" else: inside end
      | Arr(len, typ) =>
        "[" + len.tostring() + " x " + typ.tostring() + "]"
      | Pointer(typ, addrspace) =>
        cases (Option<Number>) addrspace:
          | some(n) => typ.tostring() + " addrspace(" + n.tostring() + ")*"
          | none => typ.tostring() + "*"
        end
      | Vector(len, typ) =>
        "<" + len.tostring() + " x " + typ.tostring() + ">"
      | TypeIdentifier(name) =>
        "%" + name
      | Metadata =>
        "metadata"
      | X86_mmx  =>
        "x86mmx"
    end
  end
end

data StructField:
  | struct-field(ty :: TypeKind, value :: ValueKind)
end

data ASMDialect:
  | ATT
  | Intel
sharing:
  tostring(self):
    cases(ASMDialect) self:
      | ATT   => ""
      | Intel => "inteldialect"
    end
  end
end

data ValueKind:
  | NullValue
  | Argument
  | BasicBlock
  | InlineASM(instructions :: String,
              constraints  :: String,
              side-effects :: Boolean,
              align-stack  :: Boolean,
              dialect      :: ASMDialect,
              metadata     :: Option<ValueKind%(is-MDNode)>)
  | MDNode
  | MDString
  | BlockAddress(function-name :: String, block-name :: String)
  | ConstantAggregateZero
  | ConstantArray(ty :: TypeKind, values :: List<ValueKind>)
  | ConstantString(value :: String)
  | ConstantDataArray
  | ConstantDataVector
  | ConstantExpr(op :: ConstantExpression)
  | ConstantFP(value :: Number)
  | ConstantInt(value :: Number)
  | ConstantPointerNull
  | ConstantStruct(fields :: List<StructField>)
  | ConstantVector(ty :: TypeKind, values :: List<ValueKind>)
  | FunctionValue
  | GlobalAlias
  | GlobalVariable(id :: String)
  | LocalVariable(id :: String)
  | UndefValue
  | Instruction
sharing:
  tostring(self) -> String:
    cases(ValueKind) self:
      | NullValue =>
        raise("NullValue not yet handled") # TODO
      | Argument  =>
        raise("Argument not yet handled") # TODO
      | BasicBlock =>
        raise("BasicBlock not yet handled") # TODO
      | InlineASM(instructions, constraints, side-effects, align-stack, dialect, metadata) =>
        raise("InlineASM not yet handled") # TODO
      | MDNode   =>
        raise("MDNode not yet handled") # TODO
      | MDString =>
        raise("MDString not yet handled") # TODO
      | BlockAddress(function-name, block-name) =>
        "blockaddress(@" + function-name + ", %" + block-name + ")"
      | ConstantAggregateZero =>
        raise("ConstantAggregateZero not yet handled") # TODO
      | ConstantArray(ty :: TypeKind, values :: List<ValueKind>) =>
        "[ "
          + for map(value from values):
               ty.tostring() + " " + value.tostring()
            end.join(", ")
          + " ]"
      | ConstantString(val) =>
        "c\"" + val + "\\00\""
      | ConstantDataArray  =>
        raise("ConstantDataArray not yet handled") # TODO
      | ConstantDataVector =>
        raise("ConstantDataVector not yet handled") # TODO
      | ConstantFP(value :: Number)  =>
        raise("ConstantFP not yet handled") # TODO
      | ConstantInt(value :: Number) =>
        value.tostring()
      | ConstantPointerNull =>
        "null"
      | ConstantStruct(fields :: List<StructField>) =>
        "{ "
          + for map(field from fields):
              cases(StructField) field:
                | struct-field(ty, value) =>
                  ty.tostring() + " " + value.tostring()
              end
            end.join(", ")
          + " }"
      | ConstantVector(ty :: TypeKind, values :: List<ValueKind>) =>
        raise("ConstantVector not yet handled") # TODO
      | FunctionValue =>
        raise("FunctionValue not yet handled") # TODO
      | GlobalAlias   =>
        raise("GlobalAlias not yet handled") # TODO
      | GlobalVariable(id :: String) =>
        "@" + id
      | LocalVariable(id :: String) =>
        "%" + id
      | UndefValue  =>
        "undef"
      | ConstantExpr(op :: ConstantExpression) =>
        op.tostring()
    end
  end
end

data Index:
  | access(ty :: TypeKind, val :: ValueKind)
sharing:
  tostring(self) -> String:
    cases(Index) self:
      | access(ty, val) => 
        ty.tostring() + " " + val.tostring()
    end
  end
end

data ConstantExpression:
  | Trunc(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | ZExt(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | SExt(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | FPToUI(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | FPToSI(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | UIToFP(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | SIToFP(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | FPTrunc(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | FPExt(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | PtrToInt(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | IntToPtr(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | BitCast(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | AddrSpaceCast(from-ty :: TypeKind, cst :: ValueKind, to-ty :: TypeKind)
  | GetElementPtr(inbounds :: Boolean, ty :: TypeKind, cst :: ValueKind, idxs :: List<Index>)
  | Select
  | ICmp(cond :: I.ICmp, ty1 :: TypeKind, val1 :: ValueKind, ty2 :: TypeKind, val2 :: ValueKind)
  | FCmp(cond :: F.FCmp, ty1 :: TypeKind, val1 :: ValueKind, ty2 :: TypeKind, val2 :: ValueKind)
  | ExtractElement(val :: ValueKind, idx :: Number)
  | InsertElement(val :: ValueKind, idx :: Number)
  | ShuffleVector
  | ExtractValue
  | InsertValue
sharing:
  tostring(self):
    cases(ConstantExpression) self:
      | Trunc(from-ty, cst, to-ty)         =>
        "trunc " 
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | ZExt(from-ty, cst, to-ty)          =>
        "zext " 
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | SExt(from-ty, cst, to-ty)          =>
        "sext "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | FPToUI(from-ty, cst, to-ty)        =>
        "fptoui "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | FPToSI(from-ty, cst, to-ty)        =>
        "fptosi "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | UIToFP(from-ty, cst, to-ty)        =>
        "uitofp "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | SIToFP(from-ty, cst, to-ty)        =>
        "sitofp "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | FPTrunc(from-ty, cst, to-ty)       =>
        "fptrunc "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | FPExt(from-ty, cst, to-ty)         =>
        "fpext "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | PtrToInt(from-ty, cst, to-ty)      =>
        "ptrtoint "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | IntToPtr(from-ty, cst, to-ty)      =>
        "inttoptr "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | BitCast(from-ty, cst, to-ty)       =>
        "bitcast "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | AddrSpaceCast(from-ty, cst, to-ty) =>
        "addrspacecast "
          + "(" + from-ty.tostring() + " " + cst.tostring() + " to " + to-ty.tostring() + ")"
      | GetElementPtr(inbounds, ty, cst, idxs) =>
        "getelementptr "
          + if inbounds: "inbounds " else: "" end
          + "("
          + ty.tostring() + "* " + cst.tostring() + ", "
          + for map(idx from idxs):
              idx.tostring()
            end.join-str(", ")
          + ")"
      | Select =>
        raise("Select not yet handled") # TODO
      | ICmp(cond, val1, val2) =>
        raise("ICmp not yet handled") # TODO
      | FCmp(cond, val1, val2) =>
        raise("FCmp not yet handled") # TODO
      | ExtractElement(val, idx) =>
        raise("ExtractElement not yet handled") # TODO
      | InsertElement(val, idx) =>
        raise("InsertElement not yet handled") # TODO
      | ShuffleVector =>
        raise("ShuffleVector not yet handled") # TODO
      | ExtractValue =>
        raise("ExtractValue not yet handled") # TODO
      | InsertValue =>
        raise("InsertValue not yet handled") # TODO
    end
  end
end

