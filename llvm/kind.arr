#lang pyret

provide *

# import "llvm.arr" as LLVM

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
  | FunctionType(ret :: TypeKind, params :: List<TypeKind>)
  | Struct(fields :: List<TypeKindField>, packed :: Bool)
  | Arr(len :: Number, typ :: TypeKind)
  | Pointer(typ :: TypeKind, addrspace :: Option<Number>)
  | Vector(len :: Number, typ :: TypeKind)
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
      | Integer(width :: Number) =>
        "i" + width.tostring()
      | FunctionType(ret :: TypeKind, params :: List<TypeKind>) =>
        ret.tostring() + " (" + params.join-str(", ") + ")"
      | Struct(fields, packed) =>
        inside = "{ " + fields.join-str(", ") + " }"
        if packed: "<" + inside + ">" else: inside end
      | Arr(len :: Number, typ :: TypeKind) =>
        "[" + len.tostring() + " x " + typ.tostring() + "]"
      | Pointer(typ, addrspace) =>
        cases (Option<Number>) addrspace:
          | some(n) => typ.tostring() + " addrspace(" + n.tostring() + ")*"
          | none => typ.tostring() + "*"  
        end
      | Vector(len, typ) =>
        "<" + len.tostring() + " x " + typ.tostring() + ">"
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
  | InlineAsm(instructions :: String, 
              constraints  :: String, 
              side-effects :: Boolean, 
              align-stack  :: Boolean,
              dialect      :: ASMDialect,
              metadata     :: Option<ValueKind<is-MDNode>>)
  | MDNode
  | MDString
  | BlockAddress(function-name :: String, block-name :: String)
  | ConstantAggregateZero
  | ConstantArray(ty :: TypeKind, values :: List<ValueKind>)
  | ConstantString(value :: String)
  | ConstantDataArray
  | ConstantDataVector
  | ConstantExpr
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
  | Instruction #(op :: LLVM.Opcode)
sharing:
  tostring(self) -> String:
    cases(ValueKind) self:
      | NullValue =>
      | Argument  =>
      | BasicBlock =>
      | InlineAsm(instructions, constraints, side-effects, align-stack, dialect, metadata) =>
      | MDNode   =>
      | MDString =>
      | BlockAddress(function-name, block-name) =>
        "blockaddress(@" + function-name + ", %" + block-name + ")"
      | ConstantAggregateZero =>
      | ConstantArray(ty :: TypeKind, values :: List<ValueKind>) =>
        "[ "
          + for map(value from values): 
               ty.tostring() + " " + value.tostring() 
            end.join(", ")
          + " ]"
      | ConstantString(val) =>
        "c\"" + val + "\00\""
      | ConstantDataArray  =>
      | ConstantDataVector =>
      | ConstantExpr =>
      | ConstantFP(value :: Number)  =>
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
      | FunctionValue =>
      | GlobalAlias   =>
      | GlobalVariable(id :: String) =>
        "@" + id
      | LocalVariable(id :: String) =>
        "%" + id
      | UndefValue  =>
        "undef"
      | Instruction =>
    end
  end
end
