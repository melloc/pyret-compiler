#lang pyret

provide *

import "llvm/llvm.arr" as L
import "llvm/kind.arr" as K
import ast as A
import "ast-anf.arr" as AN
import "ast-lower.arr" as AL
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


fun exit-to(instrs :: List<L.Instruction>, label :: String) -> List<L.Instruction>:
  instrs.append(L.BrUnconditional(label))
end

word-star = K.Pointer(K.Integer(64), none)


fun l-lettable-to-llvm(l :: AL.Lettable, adts :: AL.ADT) -> H.Pair<L.Instruction, L.OpCode>:
  cases(AL.Lettable) l:
    | l-application(f, args) => 
      H.pair(empty, L.Call(false, L.CCC, word-star, f, for map(arg from args):
        L.ArgPair(word-star, arg)
      end, empty))
    | l-select(field, id, rep) => 
      select-suffix = gensym("-select-var")
      load-address  = "load-address"  + select-suffix
      local-address = "local-address" + select-suffix
      H.pair(
        [ L.Assign(load-address, 
                   L.GetElementPtr(false, word-star, id, [H.pair(K.Integer(8), field)]))
        ], L.Load(rep.totype(), load-address))
  end
end

fun l-expr-to-llvm(e :: AL.Expression, adts :: List<AL.ADT>) -> List<L.Instruction>:
  cases(AL.Expression) e:
    | l-switch(value, branches, default) => 
      case-label-suffix = gensym("-case-label")
      with-labels = for map(branch from branches): 
        H.pair(gensym("branch-") + case-label-suffix, branch) 
      end
      switch-op   = L.Switch(K.Integer(32), value, for map(labeled-branch from with-labels):
        cases(H.Pair) labeled-branch:
          | pair(label, branch) => 
            L.switch-branch(K.Integer(32), branch.constructor.toint(), label)
        end
      end)
      labeled-branches = for map(labeled-branch from branches):
        cases(H.Pair) labeled-branch:
          | pair(label, branch) =>
            link(L.Label(label), l-expr-to-llvm(branch, adts))
        end
      end
      end-label = L.Label("end" + case-label-suffix)
      link(switch-op, H.flatten(labeled-branches))
    | l-let(binding, exp, body) => 
      cases(H.Pair) l-lettable-to-llvm(exp, adts):
        | pair(instrs, op) => 
          instrs.append(link(L.Assign(binding.id, op), l-expr-to-llvm(body, adts)))
      end
    | l-if(cond, consq, altern) =>
      if-suffix     = gensym("-if-label")
      consq-label   = "consq"  + if-suffix
      altern-label  = "altern" + if-suffix
      cond-split    = L.BrConditional(cond, consq-label, altern-label)
      consq-branch  = link(L.Label(consq-label), l-expr-to-llvm(consq, adts))
      altern-branch = link(L.Label(altern-label), l-expr-to-llvm(altern, adts))
      end-label     = "end" + if-suffix
      link(cond-split, H.flatten(
        [
          exit-to(consq-branch, end-label), 
          exit-to(altern-branch, end-label),
          [L.Label(end-label)]
        ]))
    | l-assign(id) => raise("assignment not yet implemented")
  end
end

fun lower-to-llvm(prog :: AL.Program) -> L.ModuleBlock:
  cases(AL.Program) prog:
    | l-prog(constants, procedures, adts) =>
      L.Module(constants, for map(proc from procedures):
        cases(AL.Procedure) proc:
          | l-proc(name, args, ret, body) =>
            arguments = for map(arg from args): 
              L.Parameter(arg.id, ann-to-type(arg.ann), [])
            end
            L.Procedure(name, ann-to-type(ret), arguments, [], l-expr-to-llvm(body, adts))
        end
      end)
  end
end

check:
    low-to-llvm(AL.l-prog([
    
    ], [], []))
end
