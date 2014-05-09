#lang pyret

provide *

import "llvm/llvm.arr" as L
import "llvm/kind.arr" as K
import "llvm/icmp.arr" as I
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
      | none    => 
        print(self.identifiers)
        raise("Couldn't find identifier `" + needle.id + "` in identifier table! Impossible to determine scope.")
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
      default-symbols = get-symbols-expr(default)
      for fold(current from default-symbols, branch from branches):
        branch-symbols = get-symbols-expr(branch.body)
        current.union(branch-symbols)
      end
    | l-let(binding, exp, body) =>
      exp-symbols  = get-symbols-lettable(exp)
      body-symbols = get-symbols-expr(body)
      exp-symbols.union(body-symbols)
    | l-seq(exp, body) =>
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
    | l-exit(message) => empty-set
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
closure-parameter = L.Parameter("env.closure", bytes, [L.Nest])
struct-pyret-value = K.TypeIdentifier("struct.pyret-value")

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
        print("Avast, thar be boogs! This suggests that there is a bug in the type inferencer and you aren't using annotations.")
        struct-pyret-value
      | t-any   =>
        struct-pyret-value
      | t-name(id)           =>
        struct-pyret-value
      | t-arrow(args, ret)   =>
        K.FunctionType(ann-to-type(ret, none, identifiers), for map(arg from args):
          ann-to-type(arg, none, identifiers)
        end, false)
      | t-method(args, ret)  =>
        K.FunctionType(ann-to-type(ret), for map(arg from args):
          ann-to-type(arg, none, identifiers)
        end, false)
      | t-record(fields)     =>
        bytes
      | t-void        =>
        K.Void
      | t-byte        =>
        byte
      | t-word        =>
        K.Integer(32)
      | t-number      =>
        struct-pyret-value
      | t-pointer(ty) =>
        K.Pointer(ann-to-type(ty, none, identifiers), none)
    end
end

fun prepend-env-to-type(ty :: K.TypeKind) -> K.TypeKind:
  cases(K.TypeKind) ty:
    | FunctionType(ret, params, is-vararg) =>
      K.FunctionType(ret, link(bytes, params), is-vararg)
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

fun initialize-constant(constant :: AC.Global) -> { globals : List<L.Global>, instrs : List<L.Instruction> }:
  cases(AC.Global) constant:
    | c-str(name, val) =>
      raise("Strings not yet supported")
    | c-num(name, val) =>

      # Prep string for initialization
      global-str   = val.tostring-fixed(10)
      num-str-name = gensym("num-init.str.")
      num-str-id   = K.GlobalVariable(num-str-name)
      num-str-ty   = K.Arr(global-str.length() + 1, byte)
      access       = K.access(K.Integer(32), K.ConstantInt(0))
      num-str-arg  = K.ConstantExpr(K.GetElementPtr(true, num-str-ty, num-str-id, [access, access]))
      visibility   = L.Default
      mode    = L.GlobalConstant
      linkage = L.Private
      value   = K.ConstantString(global-str)
      globals = [
          L.GlobalDecl(num-str-name, linkage, visibility, none, none, true, mode, num-str-ty, some(value), none, none)
      ]

      # Prep initialization instructions
      num-name     = gensym("num-init.")
      num-id       = K.LocalVariable(num-name)
      global-ty    = K.Pointer(struct-pyret-value, none)
      global-id    = K.GlobalVariable(name.id)
      instrs  = [
        L.Assign(num-name, L.Call(false, L.CCC, struct-pyret-value,
          K.GlobalVariable("initialize-integer"),
          [
            L.Argument(bytes, num-str-arg, [])
          ], [])),
        L.NoAssign(L.Store(false, struct-pyret-value, num-id, global-ty, global-id, none, none))
      ]
      { globals : globals, instrs : instrs }
  end
end

fun l-constant-to-llvm(constant :: AC.Global) -> L.Global:
  var-name   = constant.name.id
  visibility = L.Default
  cases(AC.Global) constant:
    | c-str(name, val) =>
      ty      = K.Arr(val.length() + 1, byte)
      mode    = L.GlobalConstant
      linkage = L.Private
      value   = K.ConstantString(val)
      L.GlobalDecl(var-name, linkage, visibility, none, none, true, mode, ty, some(value), none, none)
    | c-num(name, val) =>
      ty      = struct-pyret-value
      mode    = L.GlobalVariable
      linkage = L.Internal
      value   = K.UndefValue
      L.GlobalDecl(var-name, linkage, visibility, none, none, false, mode, ty, some(value), none, none)
  end
end

fun l-value-to-llvm(v :: AL.Value, symbols :: FieldSymbolTable, identifiers :: IdentifierTable, adts :: List<AL.ADT>) -> H.Pair<List<L.Instruction>, L.OpCode>:
  cases(AL.Value) v:
    | l-closure(f, env) =>
      # Generate all identifiers and types needed 
      f-ty   = ann-to-type(f.ty, some(f), identifiers)
      f-type = prepend-env-to-type(f-ty)
      f-id   = mk-llvm-variable(f, identifiers)
      tramp-ptr    = gensym("tramp-ptr.")
      tramp-ptr-id = K.LocalVariable(tramp-ptr)
      adjusted-tramp    = gensym("adjusted-tramp.")
      adjusted-tramp-id = K.LocalVariable(adjusted-tramp)
      closure-ptr = gensym("closure-ptr.")

      # Start creating instructions for making trampoline
      malloc-instr = L.Assign(tramp-ptr, malloc(10))
      init-call = L.NoAssign(L.Call(false, L.CCC, K.Void, 
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
      H.pair(instrs, L.BitCast(bytes, adjusted-tramp-id, f-ty))
    | l-boxed           =>
  end
end

fun lookup-and-cast(table-id :: K.ValueKind, field-name :: AC.Field, to-type :: K.TypeKind, symbols :: FieldSymbolTable) -> H.Pair<List<L.Instruction>, L.OpCode>:
  result-name  = gensym("table-lookup-result.")
  result-id    = K.LocalVariable(result-name)
  call-op      = L.Call(false, L.CCC, bytes, K.GlobalVariable("table-lookup"), [
    L.Argument(bytes, table-id, empty),
    L.Argument(K.Integer(64), K.ConstantInt(symbols.lookup(field-name.name)), empty)
  ], empty)
  call-instr   = L.Assign(result-name, call-op)
  bitcast-name = gensym("bitcast.")
  bitcast-id   = K.LocalVariable(bitcast-name)
  cast-op      = L.BitCast(bytes, result-id, K.Pointer(to-type, none))
  cast-instr   = L.Assign(bitcast-name, cast-op)
  load-op      = L.Load(K.Pointer(to-type, none), bitcast-id)
  H.pair([call-instr, cast-instr], load-op)
end

fun l-lettable-to-llvm(l :: AL.Lettable, symbols :: FieldSymbolTable, identifiers :: IdentifierTable, adts :: List<AL.ADT>) -> H.Pair<List<L.Instruction>, L.OpCode>:
  cases(AL.Lettable) l:
    | l-undefined => raise("l-undefined not handled")
    | l-update(table, field-name, value) =>
      table-id   = mk-llvm-variable(table, identifiers)
      table-space-name = gensym("table-space.")
      table-space-id   = K.LocalVariable(table-space-name)
      from-id          = mk-llvm-variable(value, identifiers)
      from-ty          = ann-to-type(value.ty, some(value), identifiers)
      from-ty-ptr      = K.Pointer(from-ty, none)
      malloc-name      = gensym("malloc.")
      malloc-id        = K.LocalVariable(malloc-name)
      bitcast-name     = malloc-name + gensym(".bitcast.")
      bitcast-id       = K.LocalVariable(bitcast-name)
      instrs = [
        L.Assign(malloc-name, malloc(16)),
        L.Assign(bitcast-name, L.BitCast(bytes, malloc-id, from-ty-ptr)),
        L.NoAssign(L.Store(false, from-ty, from-id, from-ty-ptr, bitcast-id, none, none)),
        L.Assign(table-space-name, L.Alloca(bytes)),
        L.NoAssign(L.Store(false, bytes, table-id, bytess, table-space-id, none, none))
      ]
      call-op    = L.Call(false, L.CCC, bytes, K.GlobalVariable("table-insert"), [
        L.Argument(bytess, table-space-id, empty),
        L.Argument(K.Integer(64), K.ConstantInt(symbols.lookup(field-name.name)), empty),
        L.Argument(bytes, malloc-id, empty)
      ], empty)
      H.pair(instrs, call-op)
    | l-lookup(table, field-name) =>
      object-id    = mk-llvm-variable(table, identifiers)
      table-name   = gensym("object-table.")
      table-id     = K.LocalVariable(table-name)
      ev           = L.Assign(table-name, L.ExtractValue(struct-pyret-value, object-id, [2]))
      # TODO: Fix this hack. We don't always want to cast to struct-pyret-value.
      std-lookup   = lookup-and-cast(table-id, field-name, struct-pyret-value, symbols)
      cases(H.Pair<List<L.Instruction>, L.OpCode>) std-lookup:
        | pair(instrs, load-op) =>
          H.pair(link(ev, instrs), load-op)
      end
    | l-env(field-name) =>
      closure-id = K.LocalVariable("env.closure")
      lookup-and-cast(closure-id, field-name, struct-pyret-value, symbols)
    | l-copy(table) =>
      table-id   = mk-llvm-variable(table, identifiers)
      result-id  = gensym("table-lookup-result.")
      call-op    = L.Call(false, L.CCC, bytes, K.GlobalVariable("table-copy"), [
        L.Argument(bytes, table-id, empty)
      ], empty)
      H.pair(empty, call-op)
    | l-box(id) =>
      H.pair(empty, L.Alloca(double-word))
    | l-unbox(bind) =>
      bind-id = mk-llvm-variable(bind, identifiers)
      bind-type = ann-to-type(bind.ty, some(bind), identifiers)
      load-op = L.Load(bind-type, bind-id)
      H.pair(empty, load-op)
    | l-application(f, args) =>
      ret-type = cases(K.TypeKind) ann-to-type(f.ty, some(f), identifiers):
        | FunctionType(ret, _, _) =>
          ret
        | else =>
          print("Warning! Applying something that might not be a function! This may be an bug in the source program, or in the inferencer. Assuming that it is a function that returns Any: " + f.ty.tostring())
          struct-pyret-value
      end
      H.pair(empty, L.Call(false, L.CCC, ret-type, mk-llvm-variable(f, identifiers), for map(arg from args):
        L.Argument(ann-to-type(arg.ty, some(arg), identifiers), mk-llvm-variable(arg, identifiers), empty)
      end, empty))
    | l-select(field, id, rep) =>
      select-suffix = gensym("-select-var.")
      load-address  = "load-address"  + select-suffix
      local-address = "local-address" + select-suffix
      H.pair(
        [ L.Assign(load-address,
                   L.GetElementPtr(false, word-star, id, [K.access(K.Integer(8), field)]))
        ], L.Load(rep.totype(), load-address))
    | l-val(val) => l-value-to-llvm(val, symbols, identifiers, adts)
  end
end

fun l-expr-to-llvm(e :: AL.Expression, symbols :: FieldSymbolTable, identifiers :: IdentifierTable, adts :: List<AL.ADT>) -> List<L.Instruction>:
  cases(AL.Expression) e:
    | l-switch(value, branches, default) => 
      # Build up all branches and labels
      case-label-suffix = gensym("-case-label.")
      with-labels = for map(branch from branches): 
        H.pair(gensym("branch-") + case-label-suffix, branch) 
      end
      # Produce jump table
      switch-branches = for map(labeled-branch from with-labels):
        cases(H.Pair) labeled-branch:
          | pair(label, branch) => 
            L.switch-branch(K.Integer(32), K.ConstantInt(branch.constructor.toint()), label)
        end
      end
      # We'll need the variant tag
      variant-name = gensym("variant-tag.")
      variant-id   = K.LocalVariable(variant-name)
      ev = L.Assign(variant-name, L.ExtractValue(struct-pyret-value, mk-llvm-variable(value, identifiers), [1]))
      # We can now create the switch instruction and build up all labels
      # and branch code
      default-label = gensym("default-") + case-label-suffix
      switch-op   = L.NoAssign(L.Switch(K.Integer(32), variant-id, default-label, switch-branches))
      labeled-branches = for map(labeled-branch from with-labels):
        cases(H.Pair) labeled-branch:
          | pair(label, branch) =>
            link(L.Label(label), l-expr-to-llvm(branch.body, symbols, identifiers, adts))
        end
      end
      default-branch = link(L.Label(default-label), l-expr-to-llvm(default, symbols, identifiers, adts))
      link(ev, link(switch-op, H.flatten(labeled-branches) + default-branch))
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
      if-suffix      = gensym("-if-label.")
      consq-label    = "consq"  + if-suffix
      altern-label   = "altern" + if-suffix
      variant-tag    = gensym("variant.boolean.")
      variant-tag-id = K.LocalVariable(variant-tag)
      tag-is-zero    = variant-tag + ".is-zero"
      tag-is-zero-id = K.LocalVariable(tag-is-zero)

      consq-branch  = link(L.Label(consq-label), l-expr-to-llvm(consq, symbols, identifiers, adts))
      altern-branch = link(L.Label(altern-label), l-expr-to-llvm(altern, symbols, identifiers, adts))
      end-label     = "end" + if-suffix
      [ 
        L.Assign(variant-tag, L.ExtractValue(struct-pyret-value, mk-llvm-variable(cond, identifiers), [1])),
        L.Assign(tag-is-zero, L.ICmp(I.Eq, K.Integer(32), variant-tag-id, K.ConstantInt(0))),
        L.NoAssign(L.BrConditional(tag-is-zero, consq-label, altern-label))
      ] + H.flatten([consq-branch, altern-branch])
    | l-assign(id, val, body) =>
      raise("Assignment not yet supported")
    | l-exit(message) =>
      stderr-name = gensym("stderr.")
      stderr-id   = K.LocalVariable(stderr-name)
      io-ty       = K.Pointer(K.TypeIdentifier("struct._IO_FILE"), none)
      fprintf-ty  = K.FunctionType(K.Integer(32), [io-ty, bytes], true)
      str-ty      = K.Arr(message.length() + 1, byte)
      str-val     = K.ConstantString(message)
      access      = K.access(K.Integer(32), K.ConstantInt(0))
      str-arg     = K.ConstantExpr(K.GetElementPtr(true, str-ty, str-val, [access, access]))
      [
        L.Assign(stderr-name, L.Load(K.Pointer(io-ty, none), K.GlobalVariable("stderr"))),
        L.NoAssign(L.Call(false, L.CCC, fprintf-ty, K.GlobalVariable("fprintf"),
          [
            L.Argument(io-ty, stderr-id, []),
            L.Argument(bytes, str-arg, [])
          ], [])),
        L.NoAssign(L.Call(false, L.CCC, K.Void, K.GlobalVariable("exit"),
          [
            L.Argument(K.Integer(32), K.ConstantInt(1), [])
          ], [L.NoReturn])),
        L.NoAssign(L.Unreachable)
      ]
    | l-ret(id)    => [L.Ret(ann-to-type(id.ty, some(id), identifiers), mk-llvm-variable(id, identifiers))]
  end
end

math-fun-type = T.t-arrow([T.t-number, T.t-number], T.t-number)

library = [
  identifier(AC.c-bind("rational-plus-method", math-fun-type), GlobalIdentifier),
  identifier(AC.c-bind("rational-minus-method", math-fun-type), GlobalIdentifier),
  identifier(AC.c-bind("rational-times-method", math-fun-type), GlobalIdentifier),
  identifier(AC.c-bind("rational-divide-method", math-fun-type), GlobalIdentifier),
  identifier(AC.c-bind("rational-equals-method", math-fun-type), GlobalIdentifier),
  identifier(AC.c-bind("rational-lte-method", math-fun-type), GlobalIdentifier),
  identifier(AC.c-bind("global.empty-table", T.t-record([])), GlobalIdentifier),
  identifier(AC.c-bind("print", T.t-arrow([T.t-any], T.t-any)), GlobalIdentifier)
]

fun lower-to-llvm(prog :: AL.Program) -> L.ModuleBlock:
  cases(AL.Program) prog:
    | l-prog(constants, procedures, adts, init) =>
      # Build up the mappings from field names to numbers
      symbols = for fold(current from set([]), procedure from procedures):
        cases(AL.Procedure) procedure:
          | l-proc(_, _, _, body, is-closure) => current.union(get-symbols-expr(body))
        end
      end.union(get-symbols-expr(init)).to-list()
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
        identifier(constant.name, GlobalIdentifier)
      end
      identifiers = identifier-table(library + proc-identifiers + constant-identifiers)

      # Convert constants and set up constant initialization logic
      llvm-globals      = constants.map(l-constant-to-llvm)
      llvm-globals-init = for fold(base from {globals : [], instrs : []}, 
                                   obj from constants.map(initialize-constant)):
        { globals : base.globals + obj.globals, instrs : base.instrs + obj.instrs }
      end

      # Convert procedures
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

print(init)
      # Create main()
      init-identifiers = identifiers
        .insert(AC.c-bind("argc", T.t-word), LocalIdentifier)
        .insert(AC.c-bind("argv", T.t-pointer(T.t-pointer(T.t-byte))), LocalIdentifier)
      init-instrs = llvm-globals-init.instrs 
        + l-expr-to-llvm(init, symbol-table, init-identifiers, adts)
      init-proc = L.Procedure("init", struct-pyret-value, [
        L.Parameter("argc", K.Integer(32), []),
        L.Parameter("argv", K.Pointer(K.Pointer(K.Integer(8), none), none), [])
      ], [L.NoUnwind], init-instrs)

      # Instantiate module
      prog-globals = llvm-globals-init.globals + llvm-globals
      prog-procs   = link(init-proc, llvm-procedures)
      L.Module(prog-globals, prog-procs)
  end
end
