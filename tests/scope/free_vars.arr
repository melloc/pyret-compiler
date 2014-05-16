#lang pyret

x = 8

fun f() -> Number:
  fun g() -> Number:
    x
  end
  g()
end

f()
