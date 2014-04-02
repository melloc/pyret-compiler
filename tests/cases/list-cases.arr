#lang pyret

data StringList:
  | string-list-link(datum :: String, rest :: StringList)
  | string-list-empty
end

fun do-cases(lst :: StringList) -> String: 
  cases (StringList) lst: 
    | string-list-link(datum, rest) => 
        "[ " + datum + " | " + do-cases(rest) + " ]"
    | string-list-empty => "[]"
  end
end

print(do-cases(string-list-empty))
print(do-cases(string-list-link("one", 
                                string-list-link("two", 
                                                 string-list-empty))))

print("Should print \"[]\", then \"[ one | [ two | [] ] ]\" on a new line.")
