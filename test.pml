infixl 7 * / rem
infixl 6 + -
infixr 5 :
infixr 4 ++
infixl 3 == <> < > <= >=
infixr 2 &&
infixr 1 ||
infixl 0 <<
infixr 0 $

# datatype boole = TRUE | FALSE
datatype a option = NONE | SOME (a ref)

let x = 20
let y = 4
in (x + y,
    x - y,
    x * y,
    x / y,
    x rem y,
    x == y,
    x <> y,
    (:)
    )
