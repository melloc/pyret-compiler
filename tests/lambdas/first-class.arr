#lang pyret

fun f():
  fun():
    8
  end
end

x = f()

print(x()) # should print 8
