#lang pyret

fun f(func :: (String -> String)) -> String:
  func("asdf")
end

print(f(fun(str :: String) -> String: str + "more" end)) # should print "asdfmore"
