#lang pyret

data TwoCases:
  | noFields
  | oneField(x :: Number)
end

fun handle-two-cases(tc :: TwoCases) -> Number:
  cases (TwoCases) tc:
    | noFields => 0
    | oneField(_) => 1
  end
end

print(handle-two-cases(noFields))
print(handle-two-cases(oneField(8)))

#print("Should print \"NF\" followed by \"OF\" on a new line")
