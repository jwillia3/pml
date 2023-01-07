# Grammar
```
    program:    infix/datatype/let
    datatype:   'datatype' id... con '=' dbind
    dbind:      con ['=' atty...] ['|' dbind]
    atty:       id | con | '(' ty,... ')'
    ty:         atty atty... ['->' ty]
    infix:      'infixl'/'infixr' int id...
    let:        'let' defs
    defs:       'rec' defs
                'and' defs
                fnrules
                atexpr '=' expr
    fnrules:    id atexpr... '=' expr ['or' fnrules]
    expr:       'if' expr 'then' expr 'else' expr
                'case' expr ('|' expr '->' expr)...
                'let' defs 'in' expr
                expr ';' expr
                infexpr
    infexpr:    atexpr [op infexpr]
    appexpr:    atexpr atexpr...
    atexpr:     int / char / string / con
                id
                '!' atexpr
                '(' expr,... ')'
                '[' expr,... ']'
                'fn' lamrules
    lamrules:   atexpr... '->' expr ['or' lamrules]
```

# NOTES
- This translates surface syntax into an enriched lambda calculus
- Types are checked from the surface syntax
- Lambda calculus is untyped
- Translating compiling patterns proved difficult
  - Augustsson (1985) works but involves a lot of code and backtracking
  - Peyton Jones, Wadler (1987)
- Ultimately, this is postponed evaluating the worth of functional
  languages in general
