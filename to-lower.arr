#lang pyret

provide *

import "llvm/llvm.arr" as L
import "llvm/kind.arr" as K
import ast as A
import "ast-anf.arr" as N
import "helpers.arr" as H

# {"nums" : set(nums),
#     "strings" : set(strings),
#     "datas" : set(datas),
#     "funcs" : set(funcs),
#     "expr" : program-insides}
# 
# Program contains:
#     - Constants
#         - Numbers
#         - Strings
#     - Procedures
# 
# Record(fields :: List<Values>)
# 
# 
# 
# Data = VariantList(List<Variant>)

data Int:
 | IntS(s :: String)
end

data AccessPath: 
  | OffP(i :: Int)
  | SelP(i :: Int, a :: AccessPath)
end

data LVariantMember: 
  | l-variant-member(name :: String, type :: ConRep)
end

data LVariant:
  | l-variant(name :: String, tag :: Int, fields :: List<LVariantMember>)
end

data LADT:
  | l-adt(variants :: LVariant)
sharing:
  lookup-variant(self, needle-name):
    cases(LADT) self:
      | l-adt(haystack) => 
        needle = list.find(fun(variant):
          cases(LVariant) variant:
            | l-variant(name, tag, fields) => name == needle-name
          end
        end, haystack)
        cases(Option<LADT>) needle:
          | some(adt) => adt
          | none => 
            raise("Variant " + needle-name + " does not exist! Bugs may exist in compiler.")
        end
    end
  end
end

data LConstant:
  | LNumber(n :: String)
  | LString(s :: String)
end

data ConRep:
  | Undecided
  | Tagged(variant :: Int)
  | Constant(variant :: Int)
  | Transparent
  | TransU
  | TransB
  | Ref
  | Variable(id :: String, ap :: AccessPath)
  | VariableC(id :: String, ap :: AccessPath)
end

data Value:
end

data LProgram:
  | l-prog(constants :: List<LConstants>, procs :: List<LProcedure>, adts :: List<LADT>)
end

data LBind:
    l-bind(id :: String, ann :: N.ABind)
end

data LProcedure:
    l-proc(args :: List<N.ABind>, ret :: A.Ann, body :: LExpression)
end

data LLettable:
  | l-application(f :: String, args :: List<String>)
  | l-select(field :: Int, id :: String)
end

data LBranch:
  | l-branch(constructor :: ConRep, code :: List<L.OpCode>)
end

data LExpression:
  | l-switch(value :: String, branches :: List<LBranch>, default :: Option<LExpression>)
  | l-let(binding :: N.ABind, e :: LLettable, body :: LExpression)
  | l-assign(binding :: String, e :: LLettable)
  | l-if(cond :: String, consq :: LExpression, altern :: LExpression)
end

fun ann-to-type(annotation :: A.Ann) -> K.TypeKind:
    cases(A.Ann) annotation:
      | a_blank => raise("Not handled")
      | a_any   => raise("Not handled")
      | a_name(l, id)           => raise("Not handled")
      | a_arrow(l, args, ret)   => K.FunctionType()
      | a_method(l, args, ret)  => K.FunctionType()
      | a_record(_, fields)     => 
        K.Struct(for map(field from fields):
          cases(A.AField) field:
            | a_field(_, name, ann) => K.TypeField(name, ann-to-type(ann))
          end
        end, false)
      | a_app(l, ann, args)     => ann-to-type(ann)
      | a_pred(l, ann, exp)     => raise("Not handled")
      | a_dot(l, obj, field)    => raise("Not handled")
    end
end

fun flatten(alol):
  for fold(base from [], current from alol):
    base.append(current)
  end
end

fun exit-to(instrs :: List<Instruction>, label :: String) -> List<Instruction>:
  instrs.append(L.BrUnconditional(label))
end

fun conrep-to-int(rep :: ConRep):
  cases(ConRep) rep:
    | Undecided                 => raise("This ConRep not implemented yet")
    | Tagged(variant :: Int)    => variant
    | Constant(variant :: Int)  => variant
    | Transparent               => raise("This ConRep not implemented yet")
    | TransU                    => raise("This ConRep not implemented yet")
    | TransB                    => raise("This ConRep not implemented yet")
    | Ref                       => raise("This ConRep not implemented yet")
    | Variable(_, _)            => raise("This ConRep not implemented yet")
    | VariableC(_, _)           => raise("This ConRep not implemented yet")
  end
end

char-star = K.Pointer(K.Integer(8), none)

fun l-lettable-to-llvm(l :: LLettable, adts :: LADT) -> L.OpCode:
  cases(LLettable) l:
    | l-application(f, args) => 
      L.Call(false, L.CCC, char-star, f, for map(arg from args):
        L.ArgPair(char-star, arg)
      end, empty)
    | l-select(field, id) => raise("l-select not yet handled") # This should go to getelementptr
  end
end

fun l-expr-to-llvm(e :: LExpression, adts :: List<LADT>) -> List<Instruction>:
  cases(LExpression) e:
    | l-switch(value, branches, default) => 
      case-label-suffix = gensym("-case-label")
      with-labels = for map(branch from branches): 
        H.pair(gensym("branch-") + case-label-suffix, branch) 
      end
      switch-op   = L.Switch(K.Integer(32), value, for map(labeled-branch from with-labels):
        cases(H.Pair) labeled-branch:
          | pair(label, branch) => 
            L.switch-branch(K.Integer(32), conrep-to-int(branch.constructor), label)
        end
      end)
      labeled-branches = for map(labeled-branch from branches):
        cases(H.Pair) labeled-branch:
          | pair(label, branch) =>
            link(L.Label(label), l-expr-to-llvm(branch, adts))
        end
      end
      end-label = L.Label("end" + case-label-suffix)
      link(switch-op, flatten(labeled-branches))
    | l-let(binding, exp, body) => 
      link(L.Assign(binding.id, l-lettable-to-llvm(exp, adts)), l-expr-to-llvm(body, adts))
    | l-if(cond, consq, altern) =>
      if-suffix     = gensym("-if-label")
      consq-label   = "consq"  + if-suffix
      altern-label  = "altern" + if-suffix
      cond-split    = L.BrConditional(cond, consq-label, altern-label)
      consq-branch  = link(L.Label(consq-label), l-expr-to-llvm(consq, adts))
      altern-branch = link(L.Label(altern-label), l-expr-to-llvm(altern, adts))
      end-label     = "end" + if-suffix
      link(cond-split, flatten(
        [
          exit-to(consq-branch, end-label), 
          exit-to(altern-branch, end-label),
          [L.Label(end-label)]
        ]))
    | l-assign(id) => raise("assignment not yet implemented")
  end
end

fun low-to-llvm(prog :: LProgram):
  cases(LProgram) prog:
    | l-prog(constants, procedures, adts) =>
      for map(proc from procedures):
        cases(LProcedure) proc:
          | l-proc(args, ret, body) =>
            arg-types = for map(arg from args): ann-to-type(arg.ann) end
            L.Procedure(
              K.FunctionType(ann-to-type(ret), arg-types), 
              l-expr-to-llvm(body, adts))
        end
      end
  end
end

check:
    low-to-llvm(l-prog(empty, empty, empty))
end
