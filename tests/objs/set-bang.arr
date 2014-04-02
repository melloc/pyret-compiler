#lang pyret

o = {mutable x : "x"}

o!{x : "y"}

print(o!x)
print("Should print \"y\"")
