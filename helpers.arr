#lang pyret

provide *

import file as F

fun <A,B> zip(a :: List<A>, b :: List<B>) -> List<Pair<A,B>>:
  cases(List<A>) a:
    | empty =>
      cases(List<B>) b:
        | empty        => empty
        | link(f2, r2) => raise("Lists not same length! Will not zip.")
      end
    | link(f1, r1) =>
      cases(List<B>) b:
        | empty        => raise("Lists not same length! Will not zip.")
        | link(f2, r2) => link(pair(f1, f2), zip(r1, r2))
      end
  end
end

fun flatten(alol):
  for fold(base from [], current from alol):
    base.append(current)
  end
end

data Pair<A,B>:
  | pair(a :: A, b :: B)
end

fun get-file-text(filename :: String) -> String:
    f = F.input-file(filename)
    prog-txt = f.read-file()
    f.close-file()
    prog-txt
end

fun put-file-text(filename :: String, txt :: String):
    f = F.output-file(filename, false)
    f.display(txt)
    f.close-file()
end
