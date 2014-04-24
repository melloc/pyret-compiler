#lang pyret

# A simple fibonacci number calculator, implemented recursively. 

fun fibs(n :: Number) -> Number: 
  if n <= 0: 
    0
  else if n == 1:
    1
  else:
    fibs(n - 1) + fibs(n - 2)
  end
end

print(fibs(0))
print(fibs(1))
print(fibs(2))
print(fibs(3))
print(fibs(4))
