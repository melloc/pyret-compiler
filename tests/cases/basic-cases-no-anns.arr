#lang pyret

data NumberList:
  | string-list-link(datum :: Number, rest :: NumberList)
  | string-list-empty
end

do-cases :: (NumberList -> Number) = fun(lst :: NumberList) -> Number: 
  cases (NumberList) lst: 
    | string-list-link(datum, rest) => 
        datum #"[ " + datum + " | " #+ do-cases(rest) + " ]"
    | string-list-empty => 5
    | else => 6
  end
end

7
