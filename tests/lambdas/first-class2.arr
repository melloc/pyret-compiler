#lang pyret

fun f(func :: (Number -> Number)) -> Number:
  func(5)
end

print(f(fun(str :: Number) -> Number: str + 8 end)) # should print "asdfmore"
