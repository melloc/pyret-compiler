#lang pyret

o = {mutable x : 11}

o!{x : 13}

print(o!x) # Should print 13
#print("Should print \"y\"")
