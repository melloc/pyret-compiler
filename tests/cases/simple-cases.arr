#lang pyret

data NumberName:
  | one
  | two
  | three
  | four
end

fun str-number-names(nn :: NumberName) -> Number:
  cases (NumberName) nn: 
    | one => 1
    | two => 2
    | else => 100
  end
end

print(str-number-names(one))
print(str-number-names(two))
print(str-number-names(three))
print(str-number-names(four))
#print("Should print \"One\", \"Two\", \"Many\", \"Many\"")
