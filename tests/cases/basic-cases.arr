#lang pyret

data NumberList:
  | string-list-link(datum :: Number, rest :: NumberList)
  | string-list-empty
end

do-cases :: (NumberList -> Number) = fun(lst :: NumberList) -> Number: 
  cases (NumberList) lst: 
    | string-list-link(datum :: Number, rest :: NumberList) => 
        datum #"[ " + datum + " | " #+ do-cases(rest) + " ]"
    | string-list-empty => 5
    | else => 6
  end
end

#print(do-cases(string-list-link(7, string-list-empty)))
print(7)
