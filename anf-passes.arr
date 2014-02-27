#lang pyret

provide *
import "ast-anf.arr" as N


next-val = (fun():
  var n = 0
  fun() -> String: 
    n := n + 1
    "id.v" + n.tostring()
  end
end)()


# Filter Lets
# I originally implemented this in terms of my intermediate representations, 
# but now it seems that we should be doing it in ANF, which is probably for 
# best in the long run (it will simplify the code significantly).
#
# TODO IMHO we should actually run this phase last, since it will get more
# done if it is run after lambdas have been removed and replaced with IDs. 
fun filter-lets(prog :: N.AProgram) -> N.AProgram:
  data Subst: 
    | let-sub(id :: String, val :: AVal)
  end

  fun lookup-in-subst(s :: String, 
                      subs :: List<Subst>,
                      alt :: AVal) -> AVal:
    cases (List<Subst>) subs: 
      | link(f,r ) =>  if f.id == s: f.val else: lookup-in-subst(s, r) end
      | empty => alt
    end
  end

  fun eval-const(expr :: AVal) -> Option<AVal>: 
    cases (AVal) expr: 
      | a-id-var(_, _) => none
      | a-id-letrec(_, _) => none # Really? What is letrec anyway? 
      | else => some(expr)
    end
  end

  fun flhelp-alettable(expr :: N.ALettable, 
                       subs :: List<Subst>) -> N.ALettable:
    cases (N.ALettable) expr:
      | a-data-expr(_,_,_,_) => raise("Not here yet")
      | a-assign(l, id, value) => 
    end
  end
end
