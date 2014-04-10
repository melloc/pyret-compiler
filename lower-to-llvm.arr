#lang pyret

provide *

import "llvm/llvm.arr" as L
import "llvm/kind.arr" as K
import ast as A
import "ast-common.arr" as AC
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

empty-set = set([])

data FieldSymbol:
  | field-symbol(field-name :: String, val :: Number)
end

data FieldSymbolTable:
  | field-symbol-table(fields :: List<FieldSymbol>)
sharing:
  lookup(self, needle :: String) -> Number:
    cases(FieldSymbolTable) self:
      | field-symbol-table(haystack) =>
        found = list.find(fun(current):
          cases(FieldSymbol) current:
            | field-symbol(field-name, val) =>
              field-name == needle
          end
        end, haystack)
        cases(Option<FieldSymbol>) found:
          | some(f) => f.val
          | none    => raise("Couldn't find symbol `" + needle + "` in symbol table!")
        end
    end
  end
end

data IdentifierScope:
  | LocalIdentifier
  | GlobalIdentifier
end

data Identifier:
  | identifier(id :: AC.Bind, scope :: IdentifierScope)
end

data IdentifierTable:
  | identifier-table(identifiers :: List<Identifier>)
sharing:
  lookup-scope(self, needle :: AC.Bind) -> Scope:
    cases(IdentifierTable) self:
      | identifier-table(haystack) =>
        found = list.find(fun(current):
          cases(Identifier) current:
            | identifier(bind, scope) =>
              bind.id == needle.id
          end
        end, haystack)
        cases(Option<Identifier>) found:
          | some(i) => i.scope
          | none    => raise("Couldn't find identifier `" + needle.id + "` in identifier table! Impossible to determine scope.")
        end
    end
  end,
  insert(self, bind :: AC.Bind, scope :: IdentifierScope):
    cases(IdentifierTable) self:
      | identifier-table(table) =>
        identifier-table(link(identifier(bind, scope), table))
    end
  end
end

fun mk-llvm-variable(bind :: AC.Bind, table :: IdentifierTable) -> K.ValueKind:
  cases(IdentifierScope) table.lookup-scope(bind):
    | LocalIdentifier  => K.LocalVariable
    | GlobalIdentifier => K.GlobalVariable
  end(bind.id)
end

fun get-symbols-lettable(expr :: AL.Lettable) -> Set<String>:
  cases(AL.Lettable) expr:
    | l-undefined                        => empty-set
    | l-update(table, field-name, value) => set([field-name.name])
    | l-lookup(table, field-name)        => set([field-name.name])
    | l-copy(table)                      => empty-set
    | l-box(id)                          => empty-set
    | l-unbox(id)                        => empty-set
    | l-application(f, args)             => empty-set
    | l-select(field, id, rep)           => empty-set
  end
end

fun get-symbols-expr(expr :: AL.Expression) -> Set<String>:
  cases(AL.Expression) expr:
    | l-switch(value, branches, default) =>
      default-symbols = cases(Option<AL.Expression>) default:
        | some(e) => get-symbols-expr(e)
        | none    => empty-set
      end
      for fold(current from default-symbols, branch from branches):
        branch-symbols = get-symbols-expr(branch.body)
        default-symbols.union(branch-symbols)
      end
    | l-let(binding, exp, body) =>
      exp-symbols  = get-symbols-lettable(exp)
      body-symbols = get-symbols-expr(body)
      exp-symbols.union(body-symbols)
    | l-if(cond, consq, altern) =>
      consq-symbols  = get-symbols-expr(consq)
      altern-symbols = get-symbols-expr(altern)
      consq-symbols.union(altern-symbols)
    | l-assign(id, val, body) =>
      get-symbols-expr(body)
    | l-ret(id)    => empty-set
  end
end

word = K.Integer(64)
double-word = K.Struct([word, word], false)

fun ann-to-type(annotation :: A.Ann) -> K.TypeKind:
    cases(A.Ann) annotation:
      | a_blank => double-word
      | a_any   => double-word
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
  instrs.append([L.BrUnconditional(label)])
end

word-star = K.Pointer(K.Integer(64), none)


fun l-lettable-to-llvm(l :: AL.Lettable, symbols :: FieldSymbolTable, identifiers :: IdentifierTable, adts :: List<AL.ADT>) -> H.Pair<List<L.Instruction>, L.OpCode>:
  cases(AL.Lettable) l:
    | l-undefined => raise("l-undefined not handled")
    | l-update(table, field-name, value) =>
      table-id   = mk-llvm-variable(table, identifiers)
      call-op    = L.Call(false, L.CCC, K.Integer(64), K.GlobalVariable("table-insert"), [
        L.Argument(K.Pointer(K.Pointer(K.Integer(8), none), none), table-id, empty), 
        L.Argument(K.Integer(64), K.ConstantInt(symbols.lookup(field-name.name)), empty),
        L.Argument(K.Integer(64), K.ConstantInt(value), empty)
      ], empty)
      H.pair(empty, call-op)
    | l-lookup(table, field-name) =>
      ### TODO: This needs more thinking
      table-id   = mk-llvm-variable(table, identifiers)
      result-id  = gensym("table-lookup-result-")
      call-op    = L.Call(false, L.CCC, K.Integer(64), K.GlobalVariable("table-lookup"), [
        L.Argument(K.Pointer(K.Pointer(K.Integer(8), none), none), table-id, empty), 
        L.Argument(K.Integer(64), K.ConstantInt(symbols.lookup(field-name.name)), empty)
      ], empty)
      call-instr = L.Assign(result-id, call-op)
      H.pair(empty, call-op)
    | l-copy(table) =>
      table-id   = mk-llvm-variable(table, identifiers)
      result-id  = gensym("table-lookup-result-")
      call-op    = L.Call(false, L.CCC, K.Pointer(K.Integer(8), none), K.GlobalVariable("table-copy"), [
        L.Argument(K.Pointer(K.Pointer(K.Integer(8), none), none), table-id, empty)
      ], empty)
      H.pair(empty, call-op)
    | l-box(id) => H.pair(empty, L.Alloca(double-word))
    | l-unbox(id) => raise("l-unbox not handled")
    | l-application(f, args) => 
      H.pair(empty, L.Call(false, L.CCC, word-star, mk-llvm-variable(f, identifiers), for map(arg from args):
        L.Argument(word-star, mk-llvm-variable(arg, identifiers), empty)
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

fun l-expr-to-llvm(e :: AL.Expression, symbols :: FieldSymbolTable, identifiers :: IdentifierTable, adts :: List<AL.ADT>) -> List<L.Instruction>:
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
            link(L.Label(label), l-expr-to-llvm(branch, symbols, adts))
        end
      end
      end-label = L.Label("end" + case-label-suffix)
      link(switch-op, H.flatten(labeled-branches))
    | l-let(binding, exp, body) => 
      updated-identifiers = identifiers.insert(binding, LocalIdentifier)
      cases(H.Pair) l-lettable-to-llvm(exp, symbols, identifiers, adts):
        | pair(instrs, op) => 
          instrs.append(link(L.Assign(binding.id, op), l-expr-to-llvm(body, symbols, updated-identifiers, adts)))
      end
    | l-if(cond, consq, altern) =>
      if-suffix     = gensym("-if-label")
      consq-label   = "consq"  + if-suffix
      altern-label  = "altern" + if-suffix
      cond-split    = L.BrConditional(cond.id, consq-label, altern-label)
      consq-branch  = link(L.Label(consq-label), l-expr-to-llvm(consq, symbols, identifiers, adts))
      altern-branch = link(L.Label(altern-label), l-expr-to-llvm(altern, symbols, identifiers, adts))
      end-label     = "end" + if-suffix
      link(cond-split, H.flatten(
        [
          exit-to(consq-branch, end-label), 
          exit-to(altern-branch, end-label),
          [L.Label(end-label)]
        ]))
    | l-assign(id) => raise("assignment not yet implemented")
    | l-ret(id)    => [L.Ret(double-word, mk-llvm-variable(id, identifiers))]
  end
end

library = [
  identifier(AC.c-bind("rational-plus-method", A.a_blank), GlobalIdentifier),
  identifier(AC.c-bind("rational-minus-method", A.a_blank), GlobalIdentifier),
  identifier(AC.c-bind("rational-times-method", A.a_blank), GlobalIdentifier),
  identifier(AC.c-bind("rational-divide-method", A.a_blank), GlobalIdentifier),
  identifier(AC.c-bind("rational-equals-method", A.a_blank), GlobalIdentifier)
]

fun lower-to-llvm(prog :: AL.Program) -> L.ModuleBlock:
  cases(AL.Program) prog:
    | l-prog(constants, procedures, adts) =>
      # Build up the mappings from field names to numbers
      symbols = for fold(current from set([]), procedure from procedures):
        cases(AL.Procedure) procedure:
          | l-proc(_, _, _, body) => current.union(get-symbols-expr(body))
        end
      end.to-list()
      var count = 0
      symbol-table = field-symbol-table(for map(symbol from symbols):
        count := count + 1
        field-symbol(symbol, count)
      end)
      # Build up initial scope
      proc-identifiers = for map(proc from procedures):
        identifier(AC.c-bind(proc.name, A.a_blank), GlobalIdentifier)
      end
      constant-identifiers = for map(constant from constants):
        identifier(AC.c-bind(constant.id, A.a_blank), GlobalIdentifier)
      end
      identifiers = identifier-table(library + proc-identifiers + constant-identifiers)
      L.Module(constants, for map(proc from procedures):
        cases(AL.Procedure) proc:
          | l-proc(name, args, ret, body) =>
            arguments = for map(arg from args): 
              L.Parameter(arg.id, ann-to-type(arg.ann), [])
            end
            func-identifiers = for fold(current from identifiers, arg from args):
              current.insert(arg, LocalIdentifier)
            end
            L.Procedure(name, ann-to-type(ret), arguments, [], l-expr-to-llvm(body, symbol-table, func-identifiers, adts))
        end
      end)
  end
end

check:
    low-to-llvm(AL.l-prog([
    
    ], [], []))
end
