#lang pyret

provide *

import "types.arr" as T
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

data Variant:
  | l-variant(name :: String, tag :: ConRep, fields :: List<AC.Field>)
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

fun find-adt(ann :: T.Type, haystack :: List<ADT>) -> ADT:
  cases(T.Type) ann:
    | t-name(id) =>
      found = list.find(fun(needle):
        needle.name == id
      end, haystack)
      cases(Option<ADT>) found:
        | some(adt) => adt
        | none      => raise("Couldn't find data type " + id + " in program!")
      end
    | else => raise("Bad annotation: " + ann.torepr())
  end
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
  | l-prog(constants :: List<AC.Global>, procs :: List<Procedure>, adts :: List<ADT>, init :: Expression)
end

data Procedure:
    l-proc(name :: String, args :: List<AC.Bind>, ret :: T.Type, body :: Expression, is-closure :: Boolean)
end

data Value:
  | l-closure(f :: AC.Bind, env :: AC.Bind)
  | l-boxed
end

data Lettable:
  | l-undefined
  | l-application(f :: AC.Bind, args :: List<AC.Bind>)
  | l-select(field :: Number, id :: String, rep :: ConRep)
  | l-update(table :: AC.Bind, field-name :: AC.Field, value :: AC.Bind)
  | l-lookup(table :: AC.Bind, field-name :: AC.Field)
  | l-env(field-name :: AC.Field)
  | l-copy(table :: AC.Bind)
  | l-box(id :: AC.Bind)
  | l-unbox(id :: AC.Bind)
  | l-val(val :: Value)
end

data Branch:
  | l-branch(constructor :: ConRep, body :: Expression)
end

data Expression:
  | l-switch(value :: AC.Bind, branches :: List<Branch>, default :: Expression)
  | l-let(binding :: AC.Bind, e :: Lettable, body :: Expression)
  | l-seq(e :: Lettable, body :: Expression)
  | l-assign(binding :: AC.Bind, val :: AC.Bind, body :: Expression)
  | l-if(cond :: AC.Bind, consq :: Expression, altern :: Expression)
  | l-exit(message :: String)
  | l-ret(id :: AC.Bind)
end


