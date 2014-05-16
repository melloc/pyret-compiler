#lang pyret

fun test(y):
  if y == 0:
    x = 9
    x + 1
  else:
    x = 5
    x + 1
  end
end

print(test(0))
print(test(1))
#print("Should output 10 and 6")
