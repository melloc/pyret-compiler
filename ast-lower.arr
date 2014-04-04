#lang pyret

provide *

import "llvm/llvm.arr" as L
import "llvm/kind.arr" as K
import "ast-anf.arr" as AN
import "ast-common.arr" as AC
import ast as A

data Int:
 | IntS(s :: String)
end

data AccessPath:
  | OffP(i :: Number)
  | SelP(i :: Number, a :: AccessPath)
end

data VariantMember:
  | l-variant-member(name :: String)
end

data Variant:
  | l-variant(name :: String, tag :: ConRep, fields :: List<VariantMember>)
end

data ADT:
  | l-adt(name :: String, variants :: List<Variant>)
sharing:
  lookup-variant(self, needle-name) -> Variant:
    cases(ADT) self:
      | l-adt(adt-name, haystack) =>
        needle = list.find(fun(variant):
          cases(Variant) variant:
            | l-variant(name, tag, fields) => name == needle-name
          end
        end, haystack)
        cases(Option<Variant>) needle:
          | some(adt) => adt
          | none =>
            raise("Variant " + needle-name + " does not exist! Bugs may exist in compiler.")
        end
    end
  end
end

fun find-adt(ann :: A.Ann, haystack :: List<ADT>) -> ADT:
  cases(A.Ann) ann:
    | a_name(l, id) =>
      found = list.find(fun(needle):
        needle.name == id
      end, haystack)
      cases(Option<ADT>) found:
        | some(adt) => adt
        | none      => raise("Couldn't find data type " + id + " in program!")
      end
    | a_pred(l, obj, field) => raise("modules not yet supported")
    | else => raise("Bad annotation: " + ann.torepr())
  end
end

data Global:
  | l-number(id :: String, n :: String)
  | l-string(id :: String, s :: String)
end

data ConRep:
  | Undecided
  | Tagged(variant :: Number)
  | Constant(variant :: Number)
  | Transparent
  | TransU
  | TransB
  | Ref
  | Variable(id :: String, ap :: AccessPath)
  | VariableC(id :: String, ap :: AccessPath)
sharing:
  toint(self) -> Number:
    cases(ConRep) self:
      | Undecided         => raise("This ConRep not implemented yet")
      | Tagged(variant)   => variant
      | Constant(variant) => variant
      | Transparent       => raise("This ConRep not implemented yet")
      | TransU            => raise("This ConRep not implemented yet")
      | TransB            => raise("This ConRep not implemented yet")
      | Ref               => raise("This ConRep not implemented yet")
      | Variable(_, _)    => raise("This ConRep not implemented yet")
      | VariableC(_, _)   => raise("This ConRep not implemented yet")
    end
  end,
  totype(self) -> K.TypeKind:
    word = K.Integer(64)
    cases(ConRep) self:
      | Undecided                 => raise("This ConRep not implemented yet")
      | Tagged(_)    =>
        K.Struct([K.TypeField("tag", word), K.TypeField("value", word)], false)
      | Constant(_)  => word
      | Transparent               => raise("This ConRep not implemented yet")
      | TransU                    => raise("This ConRep not implemented yet")
      | TransB                    => raise("This ConRep not implemented yet")
      | Ref                       => raise("This ConRep not implemented yet")
      | Variable(_, _)            => raise("This ConRep not implemented yet")
      | VariableC(_, _)           => raise("This ConRep not implemented yet")
    end
  end
end

data Program:
  | l-prog(constants :: List<Global>, procs :: List<Procedure>, adts :: List<ADT>)
end

data Procedure:
    l-proc(name :: String, args :: List<AC.Bind>, ret :: A.Ann, body :: Expression)
end

data Lettable:
  | l-undefined
  | l-application(f :: AC.Bind, args :: List<String>)
  | l-select(field :: Number, id :: String, rep :: ConRep)
  | l-update(table :: AC.Bind, field-name :: AC.Bind, value :: String)
  | l-lookup(table :: AC.Bind, field-name :: AC.Bind)
  | l-copy(table :: AC.Bind)
  | l-id(id :: AC.Bind)
  | l-box(id :: AC.Bind)
  | l-unbox(id :: AC.Bind)
end

data Branch:
  | l-branch(constructor :: ConRep, code :: Expression)
end

data Expression:
  | l-switch(value :: AC.Bind, branches :: List<Branch>, default :: Option<Expression>)
  | l-let(binding :: AC.Bind, e :: Lettable, body :: Expression)
  | l-seq(e :: Lettable, body :: Expression)
  | l-assign(binding :: AC.Bind, val :: String, body :: Expression)
  | l-if(cond :: AC.Bind, consq :: Expression, altern :: Expression)
  | l-ret(id :: AC.Bind)
end


