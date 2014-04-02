#lang pyret

data NumberName:
  | one
  | two
  | three
  | four
end

fun str-number-names(nn :: NumberName) -> String:
  cases (NumberName) nn: 
    | one => "One"
    | two => "Two"
    | else => "Many"
  end
end

print(str-number-names(one))
print(str-number-names(two))
print(str-number-names(three))
print(str-number-names(four))
print("Should print \"One\", \"Two\", \"Many\", \"Many\"")
