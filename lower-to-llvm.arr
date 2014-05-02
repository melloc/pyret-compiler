#lang pyret

provide *

import "llvm/llvm.arr" as L
import "llvm/kind.arr" as K
import ast as A
import "types.arr" as T
import "ast-common.arr" as AC
import "ast-anf.arr" as AN
import "ast-lower.arr" as AL
import "helpers.arr" as H

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
  lookup-identifier(self, needle :: AC.Bind) -> Option<Identifier>:
    cases(IdentifierTable) self:
      | identifier-table(haystack) =>
        list.find(fun(current):
          cases(Identifier) current:
            | identifier(bind, scope) =>
              bind.id == needle.id
          end
        end, haystack)
    end
  end,
  lookup-type(self, needle :: AC.Bind) -> T.Type:
    cases(Option<Identifier>) self.lookup-identifier(needle):
      | some(i) => i.id.ty
      | none    => raise("Couldn't find identifier `" + needle.id + "` in identifier table! Impossible to determine type.")
    end
  end,
  lookup-scope(self, needle :: AC.Bind) -> Scope:
    cases(Option<Identifier>) self.lookup-identifier(needle):
      | some(i) => i.scope
      | none    => raise("Couldn't find identifier `" + needle.id + "` in identifier table! Impossible to determine scope.")
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
    | l-env(field-name)                  => set([field-name.name])
    | l-copy(table)                      => empty-set
    | l-box(id)                          => empty-set
    | l-unbox(id)                        => empty-set
    | l-application(f, args)             => empty-set
    | l-select(field, id, rep)           => empty-set
    | l-val(val)                         => empty-set
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
        current.union(branch-symbols)
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

# Common LLVM values
word = K.Integer(64)
word-star = K.Pointer(word, none)
byte = K.Integer(8)
bytes = K.Pointer(byte, none)
bytess = K.Pointer(bytes, none)
double-word = K.Struct([word, word], false)
closure-type = K.Struct([word, bytes], false)
closure-parameter = L.Parameter("env.closure", bytess, [L.Nest])
struct-pyret-value = K.Struct([K.Integer(32), K.Integer(32), bytes], false)

fun ann-to-type(type :: T.Type, fallback :: Option<AC.Bind>, identifiers :: IdentifierTable) -> K.TypeKind:
    # Make sure we have a usable type
    var selected-type = type
    when T.is-t-blank(selected-type):
      cases(Option<AC.Bind>) fallback:
        | some(b) =>
          print("Discovered an untyped identifier! This suggests a bug in the type inferencer. Attempting lookup of `" + b.id + "'...")
          selected-type := identifiers.lookup-type(b)
        | none    =>
          raise("Discovered an untyped identifier! This suggests a bug in the type inferencer.")
      end
      print(" Attempting lookup.")
    end
    cases(T.Type) selected-type:
      | t-blank =>
        raise("Avast, thar be boogs! This suggests that there is a bug in the type inferencer and you aren't using annotations.")
      | t-any   =>
        struct-pyret-value
      | t-name(id)           =>
        struct-pyret-value
      | t-arrow(args, ret)   =>
        K.FunctionType(ann-to-type(ret, none, identifiers), for map(arg from args):
          ann-to-type(arg, none, identifiers)
        end)
      | t-method(args, ret)  =>
        K.FunctionType(ann-to-type(ret), for map(arg from args):
          ann-to-type(arg, none, identifiers)
        end)
      | t-record(fields)     =>
        K.Struct(for map(field from fields):
          cases(T.TypeField) field:
            | t-field(_, name, ann) => K.TypeField(name, ann-to-type(ann, none, identifiers))
          end
        end, false)
      | t-void        =>
        K.Void
      | t-byte        =>
        byte
      | t-word        =>
        K.Integer(32)
      | t-number      =>
        struct-pyret-value
      | t-pointer(ty) =>
        K.Pointer(ann-to-type(ty, none, identifiers))
    end
end

fun prepend-env-to-type(ty :: K.TypeKind) -> K.TypeKind:
  cases(K.TypeKind) ty:
    | FunctionType(ret, params) =>
      K.FunctionType(ret, link(bytes, params))
    | else =>
      raise("LLVM type not capable of having argument prepended: " + ty.tostring())
  end
end

fun malloc(size :: Number) -> L.OpCode:
  L.Call(false, L.CCC, bytes, K.GlobalVariable("malloc"),
    [L.Argument(word, K.ConstantInt(size), [])], [L.NoAlias, L.NoUnwind])
end

fun exit-to(instrs :: List<L.Instruction>, label :: String) -> List<L.Instruction>:
  instrs.append([L.BrUnconditional(label)])
end



fun l-value-to-type(v :: AL.Value) -> K.TypeKind:
  cases(AL.Value) v:
    | l-closure(f, env) => closure-type
    | l-boxed           =>
  end
end

fun l-constant-to-llvm(constant :: AC.Global) -> L.Global:
  ty = cases(AC.Global) constant:
    | c-str(name, val) =>
      K.Arr(val.length() + 1, K.Integer(8))
    | c-num(name, val) =>
      struct-pyret-value
  end
  mode = cases(AC.Global) constant:
    | c-str(name, val) =>
      L.GlobalConstant(K.ConstantString(val))
    | c-num(name, val) =>
      L.GlobalVariable
  end
  L.GlobalDecl(constant.name.id, L.Internal, L.Default, none, none, mode, ty, none, none)
end

fun l-value-to-llvm(v :: AL.Value, symbols :: FieldSymbolTable, identifiers :: IdentifierTable, adts :: List<AL.ADT>) -> H.Pair<List<L.Instruction>, L.OpCode>:
  cases(AL.Value) v:
    | l-closure(f, env) =>
      # Generate all identifiers and types needed 
      f-type = prepend-env-to-type(ann-to-type(f.ty, some(f), identifiers))
      f-id   = mk-llvm-variable(f, identifiers)
      tramp-ptr    = gensym("tramp-ptr.")
      tramp-ptr-id = K.LocalVariable(tramp-ptr)
      adjusted-tramp    = gensym("adjusted-tramp.")
      adjusted-tramp-id = K.LocalVariable(adjusted-tramp)
      closure-ptr = gensym("closure-ptr.")

      # Start creating instructions for making trampoline
      malloc-instr = L.Assign(tramp-ptr, malloc(10))
      init-call = L.NoAssign(L.Call(false, L.CCC, bytes, 
        K.GlobalVariable("llvm.init.trampoline"),
        [
          L.Argument(bytes, tramp-ptr-id, []),
          L.Argument(bytes, K.ConstantExpr(K.BitCast(f-type, f-id, bytes)), []),
          L.Argument(bytes, mk-llvm-variable(env, identifiers), [])
        ], []))
      adjust-call = L.Assign(adjusted-tramp, L.Call(false, L.CCC, bytes, 
        K.GlobalVariable("llvm.adjust.trampoline"),
        [
          L.Argument(bytes, tramp-ptr-id, [])
        ], []))
      instrs = [malloc-instr, init-call, adjust-call]
      H.pair(instrs, L.BitCast(bytes, adjusted-tramp-id, f-type))
    | l-boxed           =>
  end
end

fun l-lettable-to-llvm(l :: AL.Lettable, symbols :: FieldSymbolTable, identifiers :: IdentifierTable, adts :: List<AL.ADT>) -> H.Pair<List<L.Instruction>, L.OpCode>:
  cases(AL.Lettable) l:
    | l-undefined => raise("l-undefined not handled")
    | l-update(table, field-name, value) =>
      table-id   = mk-llvm-variable(table, identifiers)
      call-op    = L.Call(false, L.CCC, K.Integer(64), K.GlobalVariable("table-insert"), [
        L.Argument(K.Pointer(K.Pointer(K.Integer(8), none), none), table-id, empty),
        L.Argument(K.Integer(64), K.ConstantInt(symbols.lookup(field-name.name)), empty),
        L.Argument(K.Integer(64), mk-llvm-variable(value, identifiers), empty)
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
    | l-env(field-name) =>
      call-op    = L.Call(false, L.CCC, K.Integer(64), K.GlobalVariable("table-lookup"), [
        L.Argument(K.Pointer(K.Pointer(K.Integer(8), none), none), K.LocalVariable("env.closure"), empty),
        L.Argument(K.Integer(64), K.ConstantInt(symbols.lookup(field-name.name)), empty)
      ], empty)
      H.pair(empty, call-op)
    | l-copy(table) =>
      table-id   = mk-llvm-variable(table, identifiers)
      result-id  = gensym("table-lookup-result-")
      call-op    = L.Call(false, L.CCC, K.Pointer(K.Integer(8), none), K.GlobalVariable("table-copy"), [
        L.Argument(K.Pointer(K.Pointer(K.Integer(8), none), none), table-id, empty)
      ], empty)
      H.pair(empty, call-op)
    | l-box(id) =>
      H.pair(empty, L.Alloca(double-word))
    | l-unbox(id) =>
      H.pair(empty, L.Load(double-word, id))
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
    | l-val(val) => l-value-to-llvm(val, symbols, identifiers, adts)
  end
end

fun l-expr-to-llvm(e :: AL.Expression, symbols :: FieldSymbolTable, identifiers :: IdentifierTable, adts :: List<AL.ADT>) -> List<L.Instruction>:
  cases(AL.Expression) e:
    | l-switch(value, branches, default) => 
      case-label-suffix = gensym("-case-label")
      with-labels = for map(branch from branches): 
        H.pair(gensym("branch-") + case-label-suffix, branch) 
      end
      switch-branches = for map(labeled-branch from with-labels):
        cases(H.Pair) labeled-branch:
          | pair(label, branch) => 
            L.switch-branch(K.Integer(32), K.ConstantInt(branch.constructor.toint()), label)
        end
      end
      switch-op   = L.Switch(K.Integer(32), mk-llvm-variable(value, identifiers), "default", switch-branches)
      labeled-branches = for map(labeled-branch from with-labels):
        cases(H.Pair) labeled-branch:
          | pair(label, branch) =>
            link(L.Label(label), l-expr-to-llvm(branch.body, symbols, identifiers, adts))
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
    | l-seq(exp, body) =>
      cases(H.Pair) l-lettable-to-llvm(exp, symbols, identifiers, adts):
        | pair(instrs, op) => 
          instrs.append(link(L.NoAssign(op), l-expr-to-llvm(body, symbols, identifiers, adts)))
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
    | l-assign(id, val, body) => []
    | l-ret(id)    => [L.Ret(ann-to-type(id.ty, some(id), identifiers), mk-llvm-variable(id, identifiers))]
  end
end

library = [
  identifier(AC.c-bind("rational-plus-method", T.t-blank), GlobalIdentifier),
  identifier(AC.c-bind("rational-minus-method", T.t-blank), GlobalIdentifier),
  identifier(AC.c-bind("rational-times-method", T.t-blank), GlobalIdentifier),
  identifier(AC.c-bind("rational-divide-method", T.t-blank), GlobalIdentifier),
  identifier(AC.c-bind("rational-equals-method", T.t-blank), GlobalIdentifier),
  identifier(AC.c-bind("rational-lte-method", T.t-blank), GlobalIdentifier),
  identifier(AC.c-bind("global.empty-table", T.t-blank), GlobalIdentifier),
  identifier(AC.c-bind("print", T.t-blank), GlobalIdentifier)
]

fun lower-to-llvm(prog :: AL.Program) -> L.ModuleBlock:
  cases(AL.Program) prog:
    | l-prog(constants, procedures, adts, init) =>
      # Build up the mappings from field names to numbers
      symbols = for fold(current from set([]), procedure from procedures):
        cases(AL.Procedure) procedure:
          | l-proc(_, _, _, body, is-closure) => current.union(get-symbols-expr(body))
        end
      end.to-list()
      var count = 0
      symbol-table = field-symbol-table(for map(symbol from symbols):
        count := count + 1
        field-symbol(symbol, count)
      end)

      # Build up initial scope
      proc-identifiers = for map(proc from procedures):
        identifier(AC.c-bind(proc.name, T.t-arrow(for map(arg from proc.args):
          arg.ty
        end, proc.ret)), GlobalIdentifier)
      end
      constant-identifiers = for map(constant from constants):
        identifier(AC.c-bind(constant.name.id, T.t-blank), GlobalIdentifier)
      end
      identifiers = identifier-table(library + proc-identifiers + constant-identifiers)

      # Convert constants and procedures
      llvm-globals    = for map(constant from constants):
        l-constant-to-llvm(constant)
      end
      llvm-procedures = for map(proc from procedures):
        cases(AL.Procedure) proc:
          | l-proc(name, args, ret, body, is-closure) =>
            new-args = for map(arg from args): 
              L.Parameter(arg.id, ann-to-type(arg.ty, some(arg), identifiers), [])
            end
            arguments = if is-closure: link(closure-parameter, new-args) else: new-args;
            func-identifiers = for fold(current from identifiers, arg from args):
              current.insert(arg, LocalIdentifier)
            end
            L.Procedure(name, ann-to-type(ret, none, identifiers), arguments, [], l-expr-to-llvm(body, symbol-table, func-identifiers, adts))
        end
      end

      # Create main()
      main-identifiers = identifiers
        .insert(AC.c-bind("argc", T.t-word), LocalIdentifier)
        .insert(AC.c-bind("argv", T.t-pointer(T.t-pointer(T.t-byte))), LocalIdentifier)
      main = L.Procedure("main", K.Integer(64), [
        L.Parameter("argc", K.Integer(32), []),
        L.Parameter("argv", K.Pointer(K.Pointer(K.Integer(8), none), none), [])
      ], [L.NoUnwind], l-expr-to-llvm(init, symbol-table, main-identifiers, adts))

      # Instantiate module
      L.Module(llvm-globals, link(main, llvm-procedures))
  end
end
