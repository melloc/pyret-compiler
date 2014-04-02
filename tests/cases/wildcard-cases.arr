#lang pyret

data TwoCases:
  | noFields
  | oneField(x :: String)
end

fun handle-two-cases(tc :: TwoCases) -> String:
  cases (TwoCases) tc:
    | noFields => "NF"
    | oneField(_) => "OF"
  end
end

print(handle-two-cases(noFields))
print(handle-two-cases(oneField("asdf")))

print("Should print \"NF\" followed by \"OF\" on a new line")
